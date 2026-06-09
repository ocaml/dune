open! Import
open Node
open Fiber.O

(* The user can wrap exceptions into the [Non_reproducible] constructor to tell Memo
   that they should not be cached. We catch them, unwrap, and re-raise without the
   wrapper. *)
exception Non_reproducible of exn

(* Classify an exception as reproducible or not, unwrapping the [Non_reproducible]
   marker. Cycle errors are treated as non-reproducible because the dependencies of a
   computation cancelled by a dependency cycle are inaccurate. *)
let classify = function
  | { Exn_with_backtrace.exn = Non_reproducible exn; backtrace }
  | { Exn_with_backtrace.exn = Cycle_error.E _ as exn; backtrace } ->
    { Exn_with_backtrace.exn; backtrace }, false
  | exn -> exn, true
;;

let report_and_collect_errors f =
  Fiber.map_reduce_errors
    (module Collect_errors_monoid)
    ~on_error:(fun exn ->
      let exn, reproducible = classify exn in
      let+ () = Error_handler.report_error exn in
      ({ exns = Exn_set.singleton exn; reproducible } : Collect_errors_monoid.t))
    f
;;

let check_point = ref (Fiber.return ())

(* The run in which we last computed a non-reproducible error. [reset_if_necessary]
   uses it to force a new run even when the invalidation is empty, so that
   non-reproducible errors (which are never restored from the cache) get recomputed. *)
let last_saw_non_reproducible_exn_at = ref Run.invalid

(* The cancelled value of a computation aborted by a dependency cycle. Its deps are
   inaccurate, so it is stored with [Deps.empty] and as a non-reproducible error (so it
   is never restored from the cache). *)
let cancelled ~dependency_cycle : Collect_errors_monoid.t =
  { exns = Exn_set.singleton (Exn_with_backtrace.capture (Cycle_error.E dependency_cycle))
  ; reproducible = false
  }
;;

(* [Changed] if [dep] is newer than [node] and [Unchanged] otherwise. *)
let dep_changed_or_not ~(node : _ Dep_node.t) ~(dep : _ Dep_node.t) : _ Changed_or_not.t =
  match Run.compare (Dep_node.last_changed_at dep) (Dep_node.last_validated_at node) with
  | Gt -> Changed
  | Eq | Lt -> Unchanged
;;

let rec restore_from_cache
  : 'i 'o. ('i, 'o) Dep_node.t -> Cycle_error.t Changed_or_not.t Fiber.t
  =
  fun node ->
  Counter.incr Metrics.Restore.nodes;
  match node.value with
  | Uninitialized -> Fiber.return Changed_or_not.Changed
  | Error { reproducible = false; _ } ->
    (* Non-reproducible errors are not restored from the cache. This includes the cycle
       errors of cancelled computations, whose deps are inaccurate ([Deps.empty]) anyway,
       so we must not use [Deps.changed_or_not] on them. *)
    Fiber.return Changed_or_not.Changed
  | Ok _ | Error { reproducible = true; _ } ->
    (* We cache reproducible errors just like normal values. We assume that all [Memo]
       computations are deterministic, which means if we rerun a computation that
       previously raised a set of errors on the same inputs then we expect to get the same
       set of errors back and might as well skip the unnecessary work. The downside is that
       if a computation is in fact non-deterministic, there is no way to force rerunning it,
       apart from changing some of its dependencies. *)
    (* Make sure [f] gets inlined to avoid unnecessary closure allocations and improve
       stack traces in profiling. *)
    Deps.changed_or_not
      node.deps
      ~f:(fun[@inline] ~ok_to_recompute_eagerly (Dep_node.T dep) ->
        (* If the [Run.is_current] check succeeds then the node must have been [Cached] in
           the current run, so there is no need to restore it (which would allocate a
           fiber). We can compare the timestamps directly. *)
        if Run.is_current (Dep_node.last_validated_at dep)
        then Fiber.return (dep_changed_or_not ~node ~dep)
        else
          consider_and_restore_from_cache_without_adding_dep dep
          >>= function
          | Unchanged ->
            (* Here [dep_changed_or_not] can return [Changed] if the [node] was skipped in
               the previous run, i.e., it was unreachable, while the [dep] wasn't skipped
               and changed. *)
            Fiber.return (dep_changed_or_not ~node ~dep)
          | Cancelled { dependency_cycle } ->
            Fiber.return (Changed_or_not.Cancelled { dependency_cycle })
          | Changed ->
            (match Spec.has_cutoff dep.spec with
             | false when not ok_to_recompute_eagerly ->
               (* If [dep] has no cutoff and [ok_to_recompute_eagerly] is not set, it is
                  sufficient to check whether [dep] is up to date. We are in the [Changed]
                  branch, which means [dep] is not up to date, and we therefore must
                  recompute the [node]. *)
               Fiber.return Changed_or_not.Changed
             | _ ->
               (* If [dep] has a cutoff predicate, it is not sufficient to check whether it
                  is up to date: even if it isn't, after we recompute it, the resulting
                  value may remain unchanged, allowing us to skip recomputing the [node].

                  If [dep] has no cutoff but [ok_to_recompute_eagerly] is set (which could
                  happen if [dep] is a direct child of a [Par] node), we eagerly recompute
                  [dep] so that its computation runs in parallel with its siblings, instead
                  of being deferred to the compute phase where it might run sequentially. We
                  still report [Changed] in this case since there is no cutoff to check. *)
               consider_and_compute_without_adding_dep dep
               >>| (function
                | Ok () ->
                  (match Spec.has_cutoff dep.spec with
                   | false -> Changed_or_not.Changed
                   | true -> dep_changed_or_not ~node ~dep)
                | Error dependency_cycle -> Cancelled { dependency_cycle })))

and compute : 'i 'o. ('i, 'o) Dep_node.t -> unit Fiber.t =
  fun node ->
  Counter.incr Metrics.Compute.nodes;
  let deps_collector = Deps_collector.create () in
  let+ res =
    report_and_collect_errors (fun () ->
      let* () = !check_point in
      Deps_collector.run deps_collector ~f:(fun () -> node.spec.f node.input))
  in
  (match res with
   | Error { reproducible = false; _ } ->
     last_saw_non_reproducible_exn_at := Run.current ()
   | Ok _ | Error { reproducible = true; _ } -> ());
  let deps = Deps_collector.get deps_collector in
  let value : _ Value.t =
    match res with
    | Ok res -> Ok res
    | Error errors -> Error errors
  in
  update_value node value ~deps

(* Validate a node that might be out of date. The node has a [Restoring] state during the
   validation. Once finished, the node's state is [Cached] (with an up to date
   [last_validated_at]) or [Out_of_date]. *)
and start_restoring : 'i 'o. ('i, 'o) Dep_node.t -> Cycle_error.t Changed_or_not.t Fiber.t
  =
  fun node ->
  let computation = Computation.create () in
  node.state <- Restoring { restore_from_cache = computation };
  Computation.force
    computation
    ~phase:Restore_from_cache
    ~dep_node:node
    (fun _stack_frame ->
       let* restore_result = restore_from_cache node in
       let+ () =
         match restore_result with
         | Unchanged ->
           validate_value node;
           Fiber.return ()
         | Cancelled { dependency_cycle } ->
           update_value node (Error (cancelled ~dependency_cycle)) ~deps:Deps.empty;
           Fiber.return ()
         | Changed ->
           node.state <- Out_of_date;
           Fiber.return ()
       in
       restore_result)

(* Recompute a node. The node has a [Computing] state during the computation. Once
   finished, the node's state is [Cached] with an up to date [last_validated_at]. *)
and start_computing : 'i 'o. ('i, 'o) Dep_node.t -> unit Fiber.t =
  fun node ->
  let computation = Computation.create () in
  node.state <- Computing { compute = computation };
  Computation.force computation ~phase:Compute ~dep_node:node (fun _stack_frame ->
    compute node)

(* Try to validate a [Cached] node without recomputing it. Once done, the node's state is
   either [Cached] with an up to date [last_validated_at] or [Out_of_date]. *)
and consider_and_restore_from_cache_without_adding_dep
  : 'i 'o. ('i, 'o) Dep_node.t -> Cycle_error.t Changed_or_not.t Fiber.t
  =
  fun node ->
  match node.state with
  | Cached ->
    Spec.notify node.spec node.input Live;
    start_restoring node
  | Restoring { restore_from_cache } ->
    Computation.read_but_first_check_for_cycles
      ~phase:Restore_from_cache
      restore_from_cache
      ~dep_node:node
    >>| (function
     | Ok res -> res
     | Error dependency_cycle -> Cancelled { dependency_cycle })
  | Not_cached | Out_of_date | Computing _ -> Fiber.return Changed_or_not.Changed

(* Recompute the node after restoring the value from cache failed. Once done, the node's
   state is [Cached] with an up-to-date [last_validated_at].

   When this function is called, the node should be in one of [Not_cached], [Out_of_date]
   or [Computing]. However (see the CR-someday below) [node.state] can also be an
   up-to-date [Cached]. We need to figure out why. *)
and consider_and_compute_without_adding_dep
  : 'i 'o. ('i, 'o) Dep_node.t -> (unit, Cycle_error.t) result Fiber.t
  =
  fun node ->
  match node.state with
  | Cached ->
    (* CR-someday amokhov: We hit this branch in [dir-targets-promotion.t] but how is this
       possible? We need to investigate. For now, we return [Ok ()] if the node turns out
       to be up to date. *)
    if not (Run.is_current (Dep_node.last_validated_at node))
    then
      Code_error.raise
        "consider_and_compute_without_adding_dep"
        [ "state", State.to_dyn node.state; "current run", Run.to_dyn (Run.current ()) ];
    Fiber.return (Ok ())
  | Restoring _ -> assert false
  | Not_cached ->
    Spec.notify node.spec node.input Live;
    start_computing node >>| Result.ok
  | Out_of_date -> start_computing node >>| Result.ok
  | Computing { compute } ->
    Computation.read_but_first_check_for_cycles ~phase:Compute compute ~dep_node:node
;;

let exec_dep_node : 'i 'o. ('i, 'o) Dep_node.t -> 'o Fiber.t =
  fun node ->
  Fiber.of_thunk (fun () ->
    (* Doing the check here before creating any fibers, rather than in
       [consider_and_restore_from_cache_without_adding_dep], is a measurable win. *)
    if Run.is_current (Dep_node.last_validated_at node)
    then
      let* () = Deps_collector.add_dep_from_caller node in
      let stack_frame = Dep_node.T node in
      Value.get_exn node.value ~map_exn:(fun exn -> Error.extend_stack exn ~stack_frame)
    else
      consider_and_restore_from_cache_without_adding_dep node
      >>= function
      | Unchanged ->
        let* () = Deps_collector.add_dep_from_caller node in
        let stack_frame = Dep_node.T node in
        Value.get_exn node.value ~map_exn:(fun exn -> Error.extend_stack exn ~stack_frame)
      | Cancelled { dependency_cycle } ->
        (* If restoring from cache failed with a cycle error, and the node's function is
           deterministic (as it should be), then we will hit the same cycle when trying to
           recompute the result. We therefore reraise the cycle error as is. Note that
           apart from saving some work, this also helps us work around the limitation of
           the cycle detection library that can't detect the same cycle twice. *)
        raise (Cycle_error.E dependency_cycle)
      | Changed ->
        consider_and_compute_without_adding_dep node
        >>= (function
         | Ok () ->
           let* () = Deps_collector.add_dep_from_caller node in
           let stack_frame = Dep_node.T node in
           Value.get_exn node.value ~map_exn:(fun exn ->
             Error.extend_stack exn ~stack_frame)
         | Error dependency_cycle -> raise (Cycle_error.E dependency_cycle)))
;;
