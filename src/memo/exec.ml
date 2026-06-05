open! Import
open Node
open Fiber.O

(* The user can wrap exceptions into the [Non_reproducible] constructor to tell Memo
   that they should not be cached. We catch them, unwrap, and re-raise without the
   wrapper. *)
exception Non_reproducible of exn

let report_and_collect_errors f =
  Fiber.map_reduce_errors
    (module Collect_errors_monoid)
    ~on_error:(fun exn ->
      let exn, reproducible =
        match exn with
        | { Exn_with_backtrace.exn = Non_reproducible exn; backtrace } ->
          { Exn_with_backtrace.exn; backtrace }, false
        | exn -> exn, true
      in
      let+ () = Error_handler.report_error exn in
      ({ exns = Exn_set.singleton exn; reproducible } : Collect_errors_monoid.t))
    f
;;

let check_point = ref (Fiber.return ())

(* The run in which we last computed a non-reproducible error. [reset_if_necessary]
   uses it to force a new run even when the invalidation is empty, so that
   non-reproducible errors (which are never restored from the cache) get recomputed. *)
let last_saw_non_reproducible_exn_at = ref Run.invalid

let rec restore_from_cache
  : 'o. cached_value:'o Cached_value.t -> 'o Cache_lookup.t Fiber.t
  =
  fun ~cached_value ->
  match cached_value.value with
  | Error { reproducible = false; _ } ->
    (* Non-reproducible errors are not restored from the cache. This includes the cycle
         errors of cancelled computations, whose deps are inaccurate ([Deps.empty]) anyway,
         so we must not use [Deps.changed_or_not] on them. *)
    Fiber.return (Cache_lookup.Out_of_date { old_value = Option.Unboxed.none })
  | Ok _ | Error { reproducible = true; _ } ->
    (* We cache reproducible errors just like normal values. We assume that
         all [Memo] computations are deterministic, which means if we rerun a
         computation that previously raised a set of errors on the same inputs
         then we expect to get the same set of errors back and might as well
         skip the unnecessary work. The downside is that if a computation is in
         fact non-deterministic, there is no way to force rerunning it, apart
         from changing some of its dependencies. *)
    let+ deps_changed =
      (* Make sure [f] gets inlined to avoid unnecessary closure allocations and improve
           stack traces in profiling. *)
      Deps.changed_or_not
        cached_value.deps
        ~f:(fun[@inline] ~ok_to_recompute_eagerly (Dep_node.T dep) ->
          match dep.state with
          | Cached_value dep_cached_value
            when Run.is_current (Cached_value.last_validated_at dep_cached_value) ->
            (* Fast path: [dep] is already up to date in the current run, so there is no
               need to restore it (which would allocate a fiber). We can compare the
               timestamps directly. *)
            (match
               Run.compare
                 (Cached_value.last_changed_at dep_cached_value)
                 (Cached_value.last_validated_at cached_value)
             with
             | Gt -> Fiber.return Changed_or_not.Changed
             | Eq | Lt -> Fiber.return Changed_or_not.Unchanged)
          | _ ->
            consider_and_restore_from_cache_without_adding_dep dep
            >>= (function
             | Ok cached_value_of_dep ->
               (* The [Changed] branch will be taken if [cached_value]'s node was skipped
                  in the previous run (it was unreachable), while [dep] wasn't skipped and
                  [cached_value_of_dep] changed. *)
               (match
                  Run.compare
                    (Cached_value.last_changed_at cached_value_of_dep)
                    (Cached_value.last_validated_at cached_value)
                with
                | Gt -> Fiber.return Changed_or_not.Changed
                | Eq | Lt -> Fiber.return Changed_or_not.Unchanged)
             | Cancelled { dependency_cycle } ->
               Fiber.return (Changed_or_not.Cancelled { dependency_cycle })
             | Out_of_date _old_value ->
               (match dep.spec.allow_cutoff with
                | No when not ok_to_recompute_eagerly ->
                  (* If [dep] has no cutoff, it is sufficient to check whether it is up to
                     date. If not, we must recompute the [cached_value]. *)
                  Fiber.return Changed_or_not.Changed
                | No ->
                  (* [dep] is a direct child of a parallel section, so recompute it eagerly
                     (in parallel with its siblings) rather than deferring. It has no
                     cutoff, so the outcome is [Changed] regardless of the new value. *)
                  consider_and_compute_without_adding_dep dep
                  >>| (function
                   | Ok (_ : _ Cached_value.t) -> Changed_or_not.Changed
                   | Error dependency_cycle ->
                     Changed_or_not.Cancelled { dependency_cycle })
                | Yes _equal ->
                  (* If [dep] has a cutoff predicate, it is not sufficient to check
                     whether it is up to date: even if it isn't, after we recompute it,
                     the resulting value may remain unchanged, allowing us to skip
                     recomputing the [cached_value]. *)
                  consider_and_compute_without_adding_dep dep
                  >>| (function
                   | Ok cached_value_of_dep ->
                     (* Note: [cached_value_of_dep.value] will be [Cancelled _] if [dep]
                        itself doesn't introduce a dependency cycle but one of its
                        transitive dependencies does. In this case, the value will be new,
                        so we will take the [Changed] branch. *)
                     (match
                        Run.compare
                          (Cached_value.last_changed_at cached_value_of_dep)
                          (Cached_value.last_validated_at cached_value)
                      with
                      | Gt -> Changed_or_not.Changed
                      | Eq | Lt -> Unchanged)
                   | Error dependency_cycle -> Cancelled { dependency_cycle }))))
    in
    (match deps_changed with
     | Unchanged ->
       cached_value.runs
       <- Run.Pair.with_last_validated_at
            cached_value.runs
            ~last_validated_at:(Run.current ());
       Cache_lookup.Ok cached_value
     | Changed -> Out_of_date { old_value = Option.Unboxed.some cached_value }
     | Cancelled { dependency_cycle } -> Cancelled { dependency_cycle })

and compute
  :  'i 'o.
     dep_node:('i, 'o) Dep_node.t
  -> old_value:'o Cached_value.t Option.Unboxed.t
  -> stack_frame:Stack_frame_with_state.t
  -> 'o Cached_value.t Fiber.t
  =
  fun ~dep_node ~old_value ~stack_frame:_ ->
  let deps_collector = Deps_collector.create () in
  let+ res =
    report_and_collect_errors (fun () ->
      let* () = !check_point in
      Deps_collector.run deps_collector ~f:(fun () -> dep_node.spec.f dep_node.input))
  in
  let value =
    match res with
    | Ok res -> Value.Ok res
    | Error errors -> Error errors
  in
  (match res with
   | Error { reproducible = false; _ } ->
     last_saw_non_reproducible_exn_at := Run.current ()
   | Ok _ | Error { reproducible = true; _ } -> ());
  let deps = Deps_collector.get deps_collector in
  Option.Unboxed.match_
    old_value
    ~none:(fun () -> Cached_value.create value ~deps)
    ~some:(fun old_cv ->
      match Cached_value.value_changed dep_node old_cv.value value with
      | true -> Cached_value.create value ~deps
      | false -> Cached_value.confirm_old_value ~deps old_cv)

and cancel_due_to_dependency_cycle
  : 'i 'o. dep_node:('i, 'o) Dep_node.t -> dependency_cycle:Cycle_error.t -> unit Fiber.t
  =
  fun ~dep_node ~dependency_cycle ->
  match dep_node.state with
  | Cached_value { value = Error { reproducible = false; _ }; _ } -> Fiber.return ()
  | Cached_value _ | Out_of_date _ ->
    dep_node.state <- Cached_value (Cached_value.create_cancelled ~dependency_cycle);
    Fiber.return ()
  | Restoring _ ->
    dep_node.state <- Cached_value (Cached_value.create_cancelled ~dependency_cycle);
    Fiber.return ()
  | Computing { compute; _ } ->
    (match Fiber.Ivar.peek compute.ivar with
     | Some _ -> Fiber.return ()
     | None ->
       let cancelled = Cached_value.create_cancelled ~dependency_cycle in
       dep_node.state <- Cached_value cancelled;
       Fiber.Ivar.fill compute.ivar cancelled)

and start_restoring
  :  'i 'o.
     dep_node:('i, 'o) Dep_node.t
  -> cached_value:'o Cached_value.t
  -> 'o Cache_lookup.t Fiber.t
  =
  fun ~dep_node ~cached_value ->
  let computation = Computation.create () in
  dep_node.state <- Restoring { restore_from_cache = computation };
  Computation.force computation ~phase:Restore_from_cache ~dep_node (fun _stack_frame ->
    let+ restore_result = restore_from_cache ~cached_value in
    Counter.incr Metrics.Restore.nodes;
    (match restore_result with
     | Ok cached_value ->
       dep_node.state <- Cached_value cached_value;
       Spec.notify dep_node.spec dep_node.input Validated
     | Cancelled { dependency_cycle } ->
       dep_node.state <- Cached_value (Cached_value.create_cancelled ~dependency_cycle);
       Spec.notify dep_node.spec dep_node.input Validated
     | Out_of_date { old_value } -> dep_node.state <- Out_of_date { old_value });
    restore_result)

and start_computing
  :  'i 'o.
     dep_node:('i, 'o) Dep_node.t
  -> old_value:'o Cached_value.t Option.Unboxed.t
  -> 'o Cached_value.t Fiber.t
  =
  fun ~dep_node ~old_value ->
  let computation = Computation.create () in
  dep_node.state <- Computing { old_value; compute = computation };
  Computation.force computation ~phase:Compute ~dep_node (fun stack_frame ->
    let+ cached_value = compute ~dep_node ~old_value ~stack_frame in
    Counter.incr Metrics.Compute.nodes;
    dep_node.state <- Cached_value cached_value;
    Spec.notify dep_node.spec dep_node.input Validated;
    cached_value)

and consider_and_restore_from_cache_without_adding_dep
  : 'i 'o. ('i, 'o) Dep_node.t -> 'o Cache_lookup.t Fiber.t
  =
  fun dep_node ->
  match dep_node.state with
  | Cached_value cached_value ->
    (* CR-someday amokhov: The happy path here is excruciatingly slow: read
         [dep_node.state], get [cached_value] via an indirection, jump through another
         hoop [cached_value.last_validated_at] and then, finally, make the
         [Run.is_current] check. We should try to find a shortcut. *)
    if Run.is_current (Cached_value.last_validated_at cached_value)
    then Fiber.return (Cache_lookup.Ok cached_value)
    else (
      Spec.notify dep_node.spec dep_node.input Live;
      start_restoring ~dep_node ~cached_value)
  | Restoring { restore_from_cache } ->
    Computation.read_but_first_check_for_cycles
      ~phase:Restore_from_cache
      restore_from_cache
      ~dep_node
    >>| (function
     | Ok res -> res
     | Error dependency_cycle -> Cancelled { dependency_cycle })
  | Out_of_date { old_value } | Computing { old_value; _ } ->
    Fiber.return (Cache_lookup.Out_of_date { old_value })

(* This function assumes that restoring the value from cache failed. This
     means we should be in two possible states: [Out_of_date] or [Computing].
     However, as it turns out (see CR-someday below), [dep_node.state] can also
     contain an up-to-date [Cached_value]. We need to figure out why. *)
and consider_and_compute_without_adding_dep
  : 'i 'o. ('i, 'o) Dep_node.t -> ('o Cached_value.t, Cycle_error.t) result Fiber.t
  =
  fun dep_node ->
  match dep_node.state with
  | Cached_value cached_value ->
    (* CR-someday amokhov: We hit this branch in [dir-targets-promotion.t] but how is
         this possible? We need to investigate. For now, I'm replacing the [assert false]
         that we had here with [return (Ok cached_value)] if the [cached_value] turns out
         to be up to date. *)
    if not (Run.is_current (Cached_value.last_validated_at cached_value))
    then
      Code_error.raise
        "consider_and_compute_without_adding_dep"
        [ "state", State.to_dyn dep_node.state
        ; "current run", Run.to_dyn (Run.current ())
        ];
    Fiber.return (Ok cached_value)
  | Restoring _ -> assert false
  | Out_of_date { old_value } ->
    (* Fire [Live] only for never-computed (fresh) nodes. A stale node ([old_value] is
         set) has already fired [Live] when it started restoring, so we don't fire again. *)
    if Option.Unboxed.is_none old_value then Spec.notify dep_node.spec dep_node.input Live;
    start_computing ~dep_node ~old_value >>| Result.ok
  | Computing { compute; _ } ->
    Computation.read_but_first_check_for_cycles ~phase:Compute compute ~dep_node
    >>= (function
     | Ok _ as result -> Fiber.return result
     | Error dependency_cycle as result ->
       let* () = cancel_due_to_dependency_cycle ~dep_node ~dependency_cycle in
       Fiber.return result)
;;

let exec_dep_node : 'i 'o. ('i, 'o) Dep_node.t -> 'o Fiber.t =
  fun dep_node ->
  Fiber.of_thunk (fun () ->
    match dep_node.state with
    | Cached_value cached_value
      when Run.is_current (Cached_value.last_validated_at cached_value) ->
      (* Fast path: the value is already up to date in the current run. Doing this check
           here, before allocating the fibers needed by
           [consider_and_restore_from_cache_without_adding_dep], is a measurable win. *)
      let* () = Deps_collector.add_dep_from_caller dep_node in
      let stack_frame = Dep_node.T dep_node in
      Value.get_exn cached_value.value ~map_exn:(fun exn ->
        Error.extend_stack exn ~stack_frame)
    | _ ->
      let* result =
        consider_and_restore_from_cache_without_adding_dep dep_node
        >>= function
        | Ok cached_value -> Fiber.return (Ok cached_value)
        | Cancelled { dependency_cycle } ->
          (* If restoring from cache failed with a cycle error, and the node's function
               is deterministic (as it should be), then we will hit the same cycle when
               trying to recompute the result. We therefore return the cycle error as is.
               Note that apart from saving some work, this also helps us work around the
               limitation of the cycle detection library that can't detect the same cycle
               twice. *)
          dep_node.state <- Cached_value (Cached_value.create_cancelled ~dependency_cycle);
          Fiber.return (Error dependency_cycle)
        | Out_of_date _old_value -> consider_and_compute_without_adding_dep dep_node
      in
      (match result with
       | Ok res ->
         let* () = Deps_collector.add_dep_from_caller dep_node in
         let stack_frame = Dep_node.T dep_node in
         Value.get_exn res.value ~map_exn:(fun exn -> Error.extend_stack exn ~stack_frame)
       | Error cycle_error -> raise (Cycle_error.E cycle_error)))
;;
