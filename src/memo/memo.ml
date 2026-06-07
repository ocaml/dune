module Metrics0 = Metrics
open Stdune
module Metrics = Metrics0
open Fiber.O
module Graph = Dune_graph.Graph
module Console = Console
module Debug = Memo_debug
module Event = Spec.Event
include Fiber

let when_ x y =
  match x with
  | true -> y ()
  | false -> return ()
;;

let of_reproducible_fiber = Fun.id

module type Input = sig
  type t

  include Table.Key with type t := t
end

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)
module Caches = struct
  let cleaners = ref []
  let register ~clear = cleaners := clear :: !cleaners
  let clear () = List.iter !cleaners ~f:(fun f -> f ())
end

(* The user can wrap exceptions into the [Non_reproducible] constructor to tell
   Memo that they shouldn't be cached. We will catch them, unwrap, and re-raise
   without the wrapper. *)
exception Non_reproducible of exn

open Node
module Error = Node.Error
module Cycle_error = Node.Cycle_error
module Table = Node.Table

let () =
  Printexc.register_printer (fun exn ->
    let dyn =
      let open Dyn in
      match exn with
      | Error.E err -> Some (variant "Memo.Error.E" [ Error.to_dyn err ])
      | Cycle_error.E frames ->
        Some (variant "Cycle_error.E" [ Cycle_error.to_dyn frames ])
      | Non_reproducible exn -> Some (variant "Memo.Non_reproducible" [ Exn.to_dyn exn ])
      | _ -> None
    in
    Option.map dyn ~f:Dyn.to_string)
;;

module Stack_frame_with_state : sig
  type phase =
    | Restore_from_cache
    | Compute

  type t

  val to_dyn : t -> Dyn.t

  (* Create a new stack frame related to restoring or computing a [dep_node]. *)
  val create : dag_node:Lazy_dag_node.t -> phase -> dep_node:_ Dep_node.t -> t
  val dep_node : t -> Dep_node.packed
  val dag_node : t -> Dag.node
  val children_added_to_dag : t -> Dag.Id.Set.t
  val record_child_added_to_dag : t -> dag_node_id:Dag.Id.t -> unit
end = struct
  type phase =
    | Restore_from_cache
    | Compute

  type t =
    { dep_node : Dep_node.packed
    ; dag_node : Lazy_dag_node.t
    ; (* This [children_added_to_dag] table serves dual purpose:

         (1) guarantee that we never add the same edge twice to the cycle
         detection graph;

         (2) to mark the "forcing" stacks in the computation graph that were
         already added to the cycle detection graph.

         For the purpose (2) a simple [bool] could suffice instead, but we can't
         ensure (1) without an explicit set representation.

         Implementation note: We use [Dag.Id.Set.t] instead of [Dag.Id.Table.t]
         for two reasons: (i) the new cycle detection algorithm reduces the size
         of the DAG, so [children_added_to_dag] will often be empty or small,
         and in these cases [Set] is fast enough; (ii) we don't have hash sets
         in Stdune and using [unit] hash maps is disturbing. *)
      mutable children_added_to_dag : Dag.Id.Set.t
    }

  let to_dyn t = Dep_node.Packed.to_dyn_without_state t.dep_node

  let create ~dag_node (_ : phase) ~dep_node =
    { dep_node = Dep_node.T dep_node; dag_node; children_added_to_dag = Dag.Id.Set.empty }
  ;;

  let dep_node t = t.dep_node
  let dag_node t = Lazy_dag_node.force t.dag_node ~dep_node:t.dep_node

  let record_child_added_to_dag t ~dag_node_id =
    t.children_added_to_dag <- Dag.Id.Set.add t.children_added_to_dag dag_node_id
  ;;

  let children_added_to_dag t = t.children_added_to_dag
end

module To_open = struct
  module Stack_frame = Stack_frame_with_state
end

open To_open
include Parallel

module Call_stack = struct
  type t = Stack_frame_with_state.t list

  (* The variable holding the call stack for the current context. *)
  let call_stack_var : t option Fiber.Var.t = Fiber.Var.create None
  let get_call_stack () = Fiber.Var.get call_stack_var >>| Option.value ~default:[]

  let get_call_stack_without_state () =
    get_call_stack () >>| List.map ~f:Stack_frame_with_state.dep_node
  ;;

  let push_frame (frame : Stack_frame_with_state.t) f =
    let* stack = get_call_stack () in
    let stack = frame :: stack in
    Fiber.Var.set call_stack_var (Some stack) (fun () -> Implicit_output.forbid f)
  ;;

  (* As soon as we hit a cycle error in the current run, we stop adding new edges to the
     cycle detection DAG. Calling [Dag.add_assuming_missing] after a cycle has been
     detected is unsafe: it can violate the DAG's internal invariants (and previously
     caused Dune to hang). This holds the first (and only) cycle error we hit in the
     current run; it is reset by [reset]. *)
  let cycle_error_in_the_current_run : Cycle_error.t option ref = ref None
  let reset_cycle_error_in_the_current_run () = cycle_error_in_the_current_run := None

  (* Record a cycle error hit during the current run, returning the first cycle error
     seen this run. Once any cycle is hit we report that same cycle for the rest of the
     run (see [cycle_error_in_the_current_run]). *)
  let note_cycle_error cycle =
    match !cycle_error_in_the_current_run with
    | Some first -> first
    | None ->
      cycle_error_in_the_current_run := Some cycle;
      cycle
  ;;

  (* Add all edges leading from the root of the call stack to [dag_node] to the cycle
     detection DAG. *)
  let add_path_to ~dag_node : (unit, Cycle_error.t) result Fiber.t =
    let+ stack = get_call_stack () in
    (match !cycle_error_in_the_current_run with
     | Some _ ->
       (* We already hit a cycle in this run, so we must not touch the DAG again (see the
          comment on [cycle_error_in_the_current_run]). Report the first cycle below. *)
       ()
     | None ->
       let rec add_path_impl stack dag_node edges_added =
         match stack with
         | [] -> Ok (), edges_added
         | frame :: stack ->
           let dag_node_id = Dag.node_id dag_node in
           let children_added_to_dag =
             Stack_frame_with_state.children_added_to_dag frame
           in
           (match Dag.Id.Set.mem children_added_to_dag dag_node_id with
            | true ->
              (* Here we know that the current [frame] has already been traversed in a
                 previous [add_path_to] call. Therefore, the DAG already contains all the
                 edges that we will discover by continuing the recursive traversal. We
                 might as well stop here and save time. *)
              Ok (), edges_added
            | false ->
              let caller_dag_node = Stack_frame_with_state.dag_node frame in
              (match Dag.add_assuming_missing caller_dag_node dag_node with
               | exception Dag.Cycle cycle ->
                 Error (List.map cycle ~f:Dag.value), edges_added
               | () ->
                 let edges_added = edges_added + 1 in
                 let not_traversed_before = Dag.Id.Set.is_empty children_added_to_dag in
                 Stack_frame_with_state.record_child_added_to_dag frame ~dag_node_id;
                 (match not_traversed_before with
                  | true -> add_path_impl stack caller_dag_node edges_added
                  | false ->
                    (* Same optimisation as above: no need to traverse again. *)
                    Ok (), edges_added)))
       in
       let result, edges_added = add_path_impl stack dag_node 0 in
       Counter.add Metrics.Cycle_detection.edges edges_added;
       (match result with
        | Ok () -> ()
        | Error cycle_error -> cycle_error_in_the_current_run := Some cycle_error));
    match !cycle_error_in_the_current_run with
    | None -> Ok ()
    | Some cycle_error -> Error cycle_error
  ;;

  (* Add a dependency on the [dep_node] from the caller, if there is one. *)
end

(* This module contains the essence of our cycle detection algorithm. Briefly,
   the idea is as follows: whenever we are about to get blocked to wait for the
   result of a computation that is currently running, we add the current call
   stack to the cycle detection DAG. If this creates a cycle, we stop and report
   a "dependency cycle" error; otherwise, we proceed with the blocking.

   Below are some notes on how/why this algorithm works.

   By "computation" we mean execution of a "shared fiber", which we represent by
   a [Computation.t]. Computations can be in one of three states: not started,
   running, and finished. Multiple readers may want the result of a computation:
   the first reader "forces" its execution (moving it to the running state), and
   subsequent readers either get blocked if the computation is still running, or
   get the cached result if the computation has finished. Blocking can lead to a
   deadlock if there is a dependency cycle between different computations. We
   therefore need to check for cycles *before* getting blocked.

   One simple algorithm to check for cycles is to create a DAG node for every
   computation, and add a DAG edge whenever a computation would like to read the
   result of another computation. If adding an edge creates a cycle, then we
   stop and report an error instead of getting blocked and deadlocked.

   A simple optimisation is to skip adding an edge when reading the result of a
   computation that has already finished, since a computation can't finish if it
   participates in a dependency cycle.

   This algorithm is simple and it works, and Memo used it in the past. However,
   the resulting DAG was often large, and so cycle detection was taking ~35% of
   incremental zero rebuilds. As a further optimisation, we developed another
   algorithm described below.

   The DAG produced by the above algorithm can contain many uninteresting nodes
   and edges. For example, every node will have at least one incoming edge that
   is added when the corresponding computation was initially "forced". But these
   "forcing edges" cannot cause deadlocks by themselves because the fiber that
   forces a computation is not blocked, so it will keep making progress until it
   encounters a blocking edge.

   Here is an optimisation idea. Since blocking edges are the real cause of
   deadlocks, we will focus our attention on them: when we hit a blocking edge,
   we will add it to the DAG *along with the path that led us to it*. This path
   is readily available to us in the form of the call stack.

   Here is a proof sketch that this algorithm finds all possible cycles. We are
   going to make use of the following three observations:

   (1) If there is a cycle, it must contain at least one blocking edge.

   (2) Every path that we are going to add to the DAG will contain a sequence of
   forcing edges followed by one blocking edge.

   (3) Every node has at most one incoming forcing edge. In other words, forcing
   edges form a forest.

   Now consider a reachable cycle in our computation graph. It contains at least
   one blocking edge (1) and some number of forcing edges. All blocking edges of
   the cycle will be added to the DAG because our algorithm unconditionally adds
   them. What about the remaining forcing edges of the cycle? We claim that they
   must be added to the DAG together with the blocking edges, because they will
   be on the corresponding call stacks. This follows from (2) and (3): indeed,
   if a blocking edge is preceded by a sequence of forcing edges on the cycle,
   then there is only one possible call stack that contains that blocking edge
   and it must pass through that sequence of forcing edges. There is no freedom
   when we retrace the forcing edges back, since there is always at most one to
   choose from. Therefore, our algorithm will add all the edges of the cycle to
   the DAG: both blocking and forcing ones. *)
module Computation = struct
  include Computation0

  (* Each computation should be forced exactly once. Not forcing it will lead to
     a deadlock. Forcing it twice will lead to [Fiber.Ivar.fill] raising. *)
  let force { ivar; dag_node } ~phase ~dep_node fiber =
    let frame = Stack_frame_with_state.create phase ~dag_node ~dep_node in
    let* result =
      (* The only reason we make the stack [frame] available to the [fiber] is
         to let the latter get the discovered dependencies [deps_rev]. *)
      Call_stack.push_frame frame (fun () -> fiber frame)
    in
    match Fiber.Ivar.peek ivar with
    | Some _ -> Fiber.return result
    | None ->
      let+ () = Fiber.Ivar.fill ivar result in
      result
  ;;

  let read_but_first_check_for_cycles { ivar; dag_node } ~phase ~dep_node =
    match Fiber.Ivar.peek ivar with
    | Some res -> Fiber.return (Ok res)
    | None ->
      let dep_node = Dep_node.T dep_node in
      let* immediate_self_cycle =
        let+ stack = Call_stack.get_call_stack () in
        let rec collect_callers_to_dep_node = function
          | [] -> None
          | frame :: rest ->
            let stack_dep_node = Stack_frame_with_state.dep_node frame in
            if Dep_node.Packed.equal stack_dep_node dep_node
            then Some []
            else (
              match collect_callers_to_dep_node rest with
              | None -> None
              | Some callers -> Some (stack_dep_node :: callers))
        in
        match collect_callers_to_dep_node stack with
        | None -> None
        | Some callers -> Some (dep_node :: callers)
      in
      (match immediate_self_cycle with
       | Some cycle -> Fiber.return (Error (Call_stack.note_cycle_error cycle))
       | None ->
         (match (phase : Stack_frame_with_state.phase) with
          | Restore_from_cache -> Counter.incr Metrics.Restore.blocked
          | Compute -> Counter.incr Metrics.Compute.blocked);
         let dag_node = Lazy_dag_node.force dag_node ~dep_node in
         Call_stack.add_path_to ~dag_node
         >>= (function
          | Ok () -> Fiber.Ivar.read ivar >>| Result.ok
          | Error _ as cycle_error -> Fiber.return cycle_error))
  ;;
end

module Error_handler : sig
  val is_set : bool Fiber.t
  val report_error : Exn_with_backtrace.t -> unit Fiber.t

  val with_error_handler
    :  (Exn_with_backtrace.t -> unit Fiber.t)
    -> (unit -> 'a Fiber.t)
    -> 'a Fiber.t
end = struct
  type t = Exn_with_backtrace.t -> unit Fiber.t

  let var : t option Fiber.Var.t = Fiber.Var.create None
  let is_set = Fiber.map (Fiber.Var.get var) ~f:Option.is_some
  let get_exn = Fiber.Var.get_exn var

  let report_error error =
    let open Fiber.O in
    let* handler = get_exn in
    let* stack = Call_stack.get_call_stack_without_state () in
    let error =
      Exn_with_backtrace.map error ~f:(fun exn ->
        List.fold_left stack ~init:exn ~f:(fun exn stack_frame ->
          Error.extend_stack exn ~stack_frame))
    in
    Fiber.map
      (Fiber.collect_errors (fun () -> handler error))
      ~f:(function
        | Ok () -> ()
        | Error e ->
          (* Unfortunately, by re-raising an exception here we're violating some
             Memo invariants and causing more confusing exceptions, but
             hopefully this code_error will be a hint. *)
          Code_error.raise
            "Memo error handler raised an exception"
            [ "exns", Dyn.list Exn_with_backtrace.to_dyn e ])
  ;;

  let deduplicate_errors f =
    let reported = ref Exn_set.empty in
    fun exn ->
      if Exn_set.mem !reported exn
      then Fiber.return ()
      else (
        reported := Exn_set.add !reported exn;
        f exn)
  ;;

  let with_error_handler t f =
    Fiber.of_thunk (fun () ->
      (* [with_error_handler] runs once for every incremental run, so calling
         [deduplicate_errors] afresh here makes sure that we re-report all
         errors*)
      let t = deduplicate_errors t in
      Fiber.bind (Fiber.Var.get var) ~f:(function
        | None -> Fiber.Var.set var (Some t) f
        | Some _handler ->
          Code_error.raise
            "Memo.run_with_error_handler: an error handler is already installed"
            []))
  ;;
end

let pp_stack () =
  let open Pp.O in
  let+ stack = Call_stack.get_call_stack () in
  Pp.vbox
    (Pp.box (Pp.text "Memoized function stack:")
     ++ Pp.cut
     ++ Pp.chain stack ~f:(fun frame -> Dyn.pp (Stack_frame.to_dyn frame)))
;;

let dump_stack () =
  let+ pp = pp_stack () in
  Console.print [ pp ]
;;

module Stack_frame = struct
  include Dep_node.Packed

  let name (Dep_node.T t) = t.spec.name
  let input (Dep_node.T t) = Dep_node.input_to_dyn t
  let to_dyn = Dep_node.Packed.to_dyn_without_state

  let as_instance_of stack_frame ~(of_ : _ Table.t) =
    Dep_node.Packed.as_instance_of stack_frame of_.spec.witness
  ;;
end

(* There are two approaches to invalidating memoization nodes. Currently, when a
   node is invalidated by calling [invalidate_dep_node], only the node itself is
   marked as "changed" (by setting its [state] to [Out_of_date]). Then the whole
   graph is marked as "possibly changed" by calling [Run.restart ()] that makes
   all remaining [last_validated_at : Run.t] values out of date in O(1) time. In
   the next run, the whole graph is traversed from top to bottom to discover
   "actual changes" and recompute all the nodes affected by these changes. One
   disadvantage of this approach is that the whole graph needs to be traversed
   even if only a small part of it depends on the set of invalidated nodes.

   An alternative approach is as follows. When [invalidate_dep_node] is called,
   recursively mark all of its reverse dependencies as "possibly changed". Then,
   in the next run, traverse only the marked part of graph (instead of the whole
   graph as we do currently). One disadvantage of this approach is that every
   node needs to store a list of its reverse dependencies, which introduces
   cyclic memory references and complicates garbage collection. Another issue is
   that recursively marking all reverse dependencies as "possibly changed" can
   still result in marking too much, because there is no way to take the cutoff
   predicate into account when propagating "possible changes".

   Is it worth switching from the current approach to the alternative? It's best
   to answer this question by benchmarking. This is not urgent but is worth
   documenting in the code. *)
let invalidate_dep_node (dep_node : _ Dep_node.t) =
  match dep_node.state with
  | Cached_value cached_value ->
    dep_node.state <- Out_of_date { old_value = Option.Unboxed.some cached_value }
  | Out_of_date { old_value = _ } -> ()
  | Restoring _ ->
    Code_error.raise
      "invalidate_dep_node called on a node in Restoring state"
      [ "dep_node", Dep_node.to_dyn_without_state dep_node ]
  | Computing _ ->
    Code_error.raise
      "invalidate_dep_node called on a node in Computing state"
      [ "dep_node", Dep_node.to_dyn_without_state dep_node ]
;;

let invalidate_store = Store.iter ~f:invalidate_dep_node

let create_with_cache
      (type i o)
      name
      ~cache
      ~input
      ~cutoff
      ~human_readable_description
      ~on_event
      (f : i -> o Fiber.t)
  : (i, o) Table.t
  =
  let spec =
    Spec.create ~name:(Some name) ~input ~cutoff ~human_readable_description ?on_event f
  in
  Caches.register ~clear:(fun () ->
    Store.clear cache;
    invalidate_store cache);
  { cache; spec }
;;

let create_with_store
      (type i)
      name
      ~store:(module S : Store_intf.S with type key = i)
      ~input
      ?cutoff
      ?human_readable_description
      ?on_event
      f
  =
  let cache = Store.make (module S) in
  create_with_cache name ~cache ~input ~cutoff ~human_readable_description ~on_event f
;;

let create
      (type i)
      name
      ~input:(module Input : Input with type t = i)
      ?(initial_store_size = 2)
      ?cutoff
      ?human_readable_description
      ?on_event
      f
  =
  (* This mutable table is safe: the implementation tracks all dependencies. *)
  let cache = Store.of_table (Stdune.Table.create (module Input) initial_store_size) in
  let input = (module Input : Store_intf.Input with type t = i) in
  create_with_cache name ~cache ~input ~cutoff ~human_readable_description ~on_event f
;;

let make_dep_node ~spec ~input : _ Dep_node.t =
  { id = Id.gen (); input; spec; state = Out_of_date { old_value = Option.Unboxed.none } }
;;

let dep_node (t : (_, _) Table.t) input =
  Store.find_or_add t.cache input ~f:(fun input -> make_dep_node ~spec:t.spec ~input)
;;

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

module Exec : sig
  val exec_dep_node : ('i, 'o) Dep_node.t -> 'o Fiber.t
end = struct
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
    :  'i 'o.
       dep_node:('i, 'o) Dep_node.t
    -> dependency_cycle:Cycle_error.t
    -> unit Fiber.t
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
      if Option.Unboxed.is_none old_value
      then Spec.notify dep_node.spec dep_node.input Live;
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
            dep_node.state
            <- Cached_value (Cached_value.create_cancelled ~dependency_cycle);
            Fiber.return (Error dependency_cycle)
          | Out_of_date _old_value -> consider_and_compute_without_adding_dep dep_node
        in
        (match result with
         | Ok res ->
           let* () = Deps_collector.add_dep_from_caller dep_node in
           let stack_frame = Dep_node.T dep_node in
           Value.get_exn res.value ~map_exn:(fun exn ->
             Error.extend_stack exn ~stack_frame)
         | Error cycle_error -> raise (Cycle_error.E cycle_error)))
  ;;
end

let exec (type i o) (t : (i, o) Table.t) i = Exec.exec_dep_node (dep_node t i)

let create_rec
      name
      ~input
      ?initial_store_size
      ?cutoff
      ?human_readable_description
      ?on_event
      f
  =
  let rec table =
    lazy
      (create
         name
         ~input
         ?initial_store_size
         ?cutoff
         ?human_readable_description
         ?on_event
         (fun input ->
            let table = Stdlib.Lazy.force table in
            f (exec table) input))
  in
  Stdlib.Lazy.force table
;;

let dump_cached_graph ?(on_not_cached = `Raise) ?(time_nodes = false) cell =
  let rec collect_graph (Dep_node.T dep_node) graph : Graph.t Fiber.t =
    let src_id = Id.to_int dep_node.id in
    match get_cached_value_in_current_run dep_node with
    | Some cached ->
      let* attributes =
        if time_nodes
        then (
          let start = Time.now () in
          (* CR-someday cmoseley: We could record errors here and include them
             as part of the graph. *)
          let+ (_ : (_, Collect_errors_monoid.t) result) =
            report_and_collect_errors (fun () -> dep_node.spec.f dep_node.input)
          in
          let runtime = Time.Span.to_secs (Time.diff (Time.now ()) start) in
          String.Map.of_list_exn [ "runtime", Graph.Attribute.Float runtime ])
        else Fiber.return String.Map.empty
      in
      let graph = Graph.add_node graph ~id:src_id ?label:dep_node.spec.name ~attributes in
      List.fold_left
        (Deps.For_debugging.to_list cached.deps)
        ~init:(Fiber.return graph)
        ~f:(fun graph (Dep_node.T dst_node as packed) ->
          let* graph = graph in
          let dst_id = Id.to_int dst_node.id in
          let graph = Graph.add_edge graph ~src_id ~dst_id in
          if Graph.has_node graph ~id:dst_id
          then Fiber.return graph
          else collect_graph packed graph)
    | None ->
      (match on_not_cached with
       | `Raise -> failwith "Memo graph contains uncached nodes"
       | `Ignore -> Fiber.return graph)
  in
  Error_handler.with_error_handler
    (fun (_ : Exn_with_backtrace.t) -> Fiber.return ())
    (fun () -> collect_graph (Dep_node.T cell) Graph.empty)
;;

let get_call_stack = Call_stack.get_call_stack_without_state

module Invalidation = struct
  (* This is currently used only for informing the user about the reason for
     restarting a build. *)
  module Reason = struct
    type t =
      | Unknown
      | Path_changed of Path.t
      | Event_queue_overflow
      | Upgrade
      | Test
      | Variable_changed of string

    let to_string_hum = function
      | Unknown -> None
      | Path_changed path -> Some (Path.to_string path ^ " changed")
      | Event_queue_overflow -> Some "Event queue overflow; full rebuild required"
      | Upgrade -> Some "Dune upgrader initiated a full rebuild"
      | Test -> Some "Rebuild initiated by an internal testsuite"
      | Variable_changed v -> Some (sprintf "Variable %s changed" v)
    ;;

    let equal a b =
      match a, b with
      | Unknown, Unknown
      | Event_queue_overflow, Event_queue_overflow
      | Upgrade, Upgrade
      | Test, Test -> true
      | Path_changed a, Path_changed b -> Path.equal a b
      | Variable_changed a, Variable_changed b -> String.equal a b
      | ( ( Unknown
          | Path_changed _
          | Event_queue_overflow
          | Upgrade
          | Test
          | Variable_changed _ )
        , _ ) -> false
    ;;
  end

  module Leaf = struct
    type kind =
      | Invalidate_node : _ Dep_node.t -> kind
      | Clear_cache : ('input, ('input, 'output) Dep_node.t) Store.t -> kind
      | Clear_caches
      | Custom of (unit -> unit)

    type t =
      { kind : kind
      ; reason : Reason.t
      }

    let to_string_hum { reason; _ } = Reason.to_string_hum reason
  end

  module T = struct
    (* Represented as a tree mainly to get a tail-recursive execution. *)
    type t =
      | Empty
      | Leaf of Leaf.t
      | Combine of t * t

    let empty : t = Empty

    let combine a b =
      match a, b with
      | Empty, x | x, Empty -> x
      | x, y -> Combine (x, y)
    ;;
  end

  include T
  include (Monoid.Make (T) : Monoid.S with type t := t)

  let execute_leaf { Leaf.kind; _ } =
    match kind with
    | Invalidate_node dep_node -> invalidate_dep_node dep_node
    | Clear_cache store -> invalidate_store store
    | Clear_caches -> Caches.clear ()
    | Custom f -> f ()
  ;;

  let rec execute x xs =
    match x with
    | Empty -> execute_list xs
    | Leaf f ->
      execute_leaf f;
      execute_list xs
    | Combine (x, y) -> execute x (y :: xs)

  and execute_list = function
    | [] -> ()
    | x :: xs -> execute x xs
  ;;

  let rec to_list_x_xs x xs acc =
    match x with
    | Empty -> to_list_xs xs acc
    | Leaf f -> to_list_xs xs (f :: acc)
    | Combine (x, y) -> to_list_x_xs x (y :: xs) acc

  and to_list_xs xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> to_list_x_xs x xs acc
  ;;

  let to_list t = to_list_x_xs t [] []

  let details_hum ?(max_elements = 1) t =
    assert (max_elements > 0);
    let details =
      List.filter_map ~f:Leaf.to_string_hum (to_list t)
      |> String.Set.of_list
      |> String.Set.to_list
    in
    (* CR-someday amokhov: Right now we just take first [max_elements] elements
       from the sorted list, but we could prioritise some reasons over others,
       e.g. if there is a global reset because of [Event_queue_overflow], it may
       be better to ensure that this reason is included. *)
    match List.truncate ~max_length:max_elements details with
    | `Not_truncated [] ->
      [ "Restarting for an unknown reason, please report it as a bug" ]
    | `Not_truncated details -> details
    | `Truncated truncated_details ->
      let extra_message =
        let remaining_details = List.length details - max_elements in
        let plural =
          match remaining_details > 1 with
          | true -> "s"
          | false -> ""
        in
        sprintf ", and %d more change%s" remaining_details plural
      in
      (match List.destruct_last truncated_details with
       | None -> assert false
       | Some (all_but_last, last) -> all_but_last @ [ last ^ extra_message ])
  ;;

  let changed_paths t =
    List.filter_map (to_list t) ~f:(fun ({ Leaf.reason; _ } : Leaf.t) ->
      match reason with
      | Path_changed path -> Some path
      | Unknown | Event_queue_overflow | Upgrade | Test | Variable_changed _ -> None)
    |> Path.Set.of_list
    |> Path.Set.to_list
  ;;

  let execute x = execute x []

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let clear_caches ~reason = Leaf { kind = Clear_caches; reason }

  let invalidate_cache ~reason ({ cache; _ } : _ Table.t) =
    Leaf { kind = Clear_cache cache; reason }
  ;;

  let invalidate_node ~reason (dep_node : _ Dep_node.t) =
    Leaf { kind = Invalidate_node dep_node; reason }
  ;;

  (* For out-of-band caching mechanisms that need to run an arbitrary action when the build
     is invalidated. *)
  let custom ~reason ~f = Leaf { kind = Custom f; reason }

  let to_reason_list t =
    List.fold_left (to_list t) ~init:[] ~f:(fun acc ({ Leaf.reason; _ } : Leaf.t) ->
      if List.mem acc reason ~equal:Reason.equal then acc else reason :: acc)
    |> List.rev
  ;;
end

module Current_run = struct
  let f () = Run.current () |> return
  let memo = create "current-run" ~input:(module Unit) f
  let exec () = exec memo ()
  let invalidate ~reason = Invalidation.invalidate_node ~reason (dep_node memo ())
end

let current_run () = Current_run.exec ()

let of_non_reproducible_fiber fiber =
  let* (_ : Run.t) = current_run () in
  fiber
;;

let is_top_level =
  let+ is_set = Error_handler.is_set in
  not is_set
;;

let run_with_error_handler t ~handle_error_no_raise =
  Error_handler.with_error_handler handle_error_no_raise (fun () ->
    let* res = report_and_collect_errors t in
    match res with
    | Ok ok -> Fiber.return ok
    | Error ({ exns; reproducible = _ } : Collect_errors_monoid.t) ->
      Fiber.reraise_all (Exn_set.to_list exns))
;;

let run t =
  let* is_top_level = is_top_level in
  (* CR-someday aalekseyev: I think this automagical detection of toplevel calls
     is weird. My hunch is that having separate functions for toplevel and
     non-toplevel [run] would be better. *)
  match is_top_level with
  | true ->
    (* Start each top-level run with a clean cycle-detection state so that a cycle hit in
       one run does not leak into independent later runs. In production Memo performs a
       single run per build with a [reset] in between, so this matches resetting in
       [reset]. (Concurrent top-level runs do not occur in Dune; if that ever changes,
       this reset would need to be scoped to the individual run rather than shared.) *)
    Call_stack.reset_cycle_error_in_the_current_run ();
    run_with_error_handler
      (fun () -> t)
      ~handle_error_no_raise:(fun _exn -> Fiber.return ())
  | false -> t
;;

module With_implicit_output = struct
  type ('i, 'o) t = 'i -> 'o Fiber.t

  let create name ~input ~implicit_output impl =
    let memo =
      create name ~input (fun i ->
        Implicit_output.collect implicit_output (fun () -> impl i))
    in
    fun input ->
      let* res, output = exec memo input in
      let+ () = Implicit_output.produce_opt implicit_output output in
      res
  ;;

  let exec t = t
end

module Cell = struct
  type ('i, 'o) t = ('i, 'o) Dep_node.t

  let input (t : (_, _) t) = t.input
  let read = Exec.exec_dep_node
  let invalidate = Invalidation.invalidate_node
end

let cell = dep_node

module Implicit_output = Implicit_output

let lazy_cell ?cutoff ?name ?human_readable_description ?on_event f =
  let on_event = Option.map on_event ~f:(fun on_event () event -> on_event event) in
  let spec =
    Spec.create ~name ~input:(module Unit) ~cutoff ~human_readable_description ?on_event f
  in
  make_dep_node ~spec ~input:()
;;

let push_stack_frame ~human_readable_description f =
  Cell.read (lazy_cell ~human_readable_description f)
;;

module Lazy = struct
  type 'a t = unit -> 'a Fiber.t

  let of_val a () = Fiber.return a

  module Expert = struct
    let create ?cutoff ?name ?human_readable_description ?on_event f =
      let cell = lazy_cell ?cutoff ?name ?human_readable_description ?on_event f in
      cell, fun () -> Cell.read cell
    ;;
  end

  let create ?cutoff ?name ?human_readable_description ?on_event f =
    let cell = lazy_cell ?cutoff ?name ?human_readable_description ?on_event f in
    fun () -> Cell.read cell
  ;;

  let force f = f ()
  let map t ~f = create (fun () -> Fiber.map ~f (t ()))
end

let lazy_ = Lazy.create

module Poly (Function : sig
    type 'a input
    type 'a output

    val name : string
    val id : 'a input -> 'a Type_eq.Id.t
    val to_dyn : _ input -> Dyn.t
    val eval : 'a input -> 'a output Fiber.t
  end) =
struct
  open Function

  module Key = struct
    type t = T : _ input -> t

    let to_dyn (T t) = to_dyn t
    let hash (T t) = Type_eq.Id.hash (id t)
    let equal (T x) (T y) = Type_eq.Id.equal (id x) (id y)
  end

  module Value = struct
    type t = T : ('a Type_eq.Id.t * 'a output) -> t

    let get (type a) ~(input_with_matching_id : a input) value : a output =
      match value with
      | T (id_v, res) ->
        (match Type_eq.Id.same id_v (id input_with_matching_id) with
         | None ->
           Code_error.raise
             "Type_eq.Id.t mismatch in Memo.Poly: the likely reason is that the provided \
              Function.id returns different ids for the same input."
             [ "Function.name", Dyn.String name ]
         | Some Type_eq.T -> res)
    ;;
  end

  let memo =
    create
      name
      ~input:(module Key)
      (function Key.T input -> eval input >>| fun v -> Value.T (id input, v))
  ;;

  let eval x = exec memo (Key.T x) >>| Value.get ~input_with_matching_id:x
end

let reset invalidation =
  (* We rely on [invalidation] to list the actual reasons for the reset, which
     justifies the [~reason:Unknown] below. *)
  let invalidate_current_run = Current_run.invalidate ~reason:Unknown in
  Invalidation.execute (Invalidation.combine invalidation invalidate_current_run);
  Call_stack.reset_cycle_error_in_the_current_run ();
  Run.restart ()
;;

(* Like [reset], but a no-op when nothing can have changed: the [invalidation] is empty and
   no non-reproducible error was computed in the current run. This lets a caller (e.g. a
   file-watcher waking up with no relevant changes) avoid advancing the run and discarding
   the whole cache. *)
let reset_if_necessary invalidation =
  if
    (not (Invalidation.is_empty invalidation))
    || Run.is_current !last_saw_non_reproducible_exn_at
  then reset invalidation
;;

module For_tests = struct
  let get_deps (type i o) (t : (i, o) Table.t) inp =
    match Store.find t.cache inp with
    | None -> None
    | Some dep_node ->
      (match get_cached_value_in_current_run dep_node with
       | None -> None
       | Some cv ->
         Some
           (Deps.For_debugging.to_list cv.deps
            |> List.map ~f:(fun (Dep_node.T dep) ->
              dep.spec.name, Dep_node.input_to_dyn dep)))
  ;;

  let clear_memoization_caches () = Caches.clear ()
end

module Store = Store_intf

module Run = struct
  type t = Run.t

  module For_tests = struct
    let compare = Run.compare
    let current = Run.current
  end
end

module type S = sig
    type 'a memo = 'a t

    include Monad.S
    module List : Monad.List with type 'a t := 'a t

    val of_memo : 'a memo -> 'a t
  end
  with type 'a memo := 'a t

let of_memo = Fun.id

module List = struct
  include Monad.List (Fiber)

  let map = parallel_map
  let concat_map l ~f = map l ~f >>| List.concat
end

module Option = Monad.Option (Fiber)
module Result = Monad.Result (Fiber)

module Var = struct
  (* CR-soon amokhov: Simplify this to [type 'a t = (unit, 'a) Cell.t].

     [Cell.t]s already store all the information we need, and the only change that needs
     to happen is making [Cell.invalidate] smart enough to return [Invalidation.empty]
     when the [cutoff] fires.

     Once we have that, we should also be able to implement [Fs_memo] on top of [Var.t]s
     instead of hand-written "variable tables".
  *)
  type 'a t =
    { cell : (unit, 'a) Cell.t
    ; value : 'a ref
      (* We manually cutoff instead of depending on [Cell.t] cutoff mechanism, so that
           we don't pay for invalidation when the value doesn't change. *)
    ; cutoff : ('a -> 'a -> bool) option
    }

  let create (type a) ?cutoff value ~name : a t =
    let value = ref value in
    let spec =
      Spec.create
        ~name:(Some name)
        ~input:(module Stdune.Unit)
        ~human_readable_description:None
        ~cutoff:None
        (fun () -> return !value)
    in
    let cell = make_dep_node ~spec ~input:() in
    { cell; value; cutoff }
  ;;

  let set t v =
    match t.cutoff with
    | Some cutoff when cutoff !(t.value) v ->
      (* Note: We do *not* set [t.value := v] when the change is insignificant according
         to the [cutoff]. This is consistent with how cutoffs work in the rest of Memo,
         e.g., see the [compute] and [confirm_old_value] functions.

         This prevents the "cutoff creep" where, e.g., the [cutoff] returns [true] for
         changes within 1% of the current [float] value and a sequence of smaller changes
         goes unnoticed, even when the aggregate change far exceeds the 1% threshold. *)
      Invalidation.empty
    | Some _ | None ->
      t.value := v;
      Cell.invalidate
        t.cell
        ~reason:(Variable_changed (Stdune.Option.value_exn t.cell.spec.name))
  ;;

  let read t = Cell.read t.cell

  module Unit = struct
    type t = (unit, unit) Cell.t

    (* All [unit]-valued variables share a single spec; they only differ by node identity,
       which is what invalidation acts on. *)
    let spec =
      Spec.create
        ~name:(Some "Memo.Var.Unit")
        ~input:(module Stdune.Unit)
        ~human_readable_description:None
        ~cutoff:None
        (fun () -> return ())
    ;;

    let create () : t = make_dep_node ~spec ~input:()
    let invalidate t ~reason = Cell.invalidate t ~reason
    let read t = Cell.read t
  end
end
