module Metrics0 = Metrics
open! Import
module Metrics = Metrics0
module Graph = Dune_graph.Graph
module Console = Console
module Debug = Memo_debug
module Event = Spec.Event
include Fiber
open Fiber.O
module Id = Id.Make ()

module M = struct
  module Import = struct
    module Dag = Dag
  end

  (* A [value] along with some additional information that allows us to check
     whether it is up to date or needs to be recomputed. *)
  module rec Cached_value : sig
    type 'a t =
      { value : 'a Value.t
      ; (* We store [last_changed_at] and [last_validated_at] for early cutoff, packed
           into a single immediate [Run.Pair.t] to save memory. See Section 5.2.2 of
           "Build Systems a la Carte: Theory and Practice" for more details
           (https://doi.org/10.1017/S0956796820000088).

           - [last_changed_at] is the run when the value changed last time.

           - [last_validated_at] is the run when the value was last confirmed as
             up to date. Invariant: [last_changed_at <= last_validated_at].

           Consider a dependency [dep] of a node [caller].

           If [dep]'s [last_changed_at] is greater than [caller]'s [last_validated_at],
           then the [dep]'s value has changed since it had been previously used by the
           [caller] and therefore the [caller] needs to be recomputed. *)
        mutable runs : Run.Pair.t
      ; (* The list of dependencies [deps], as captured at [last_validated_at]. Note that
           the list of dependencies can change over the lifetime of [Cached_value]: this
           happens if the value gets re-computed but is declared unchanged by the cutoff
           check.

           Note that [deps] should be listed in the order in which they were
           depended on to avoid recomputations of the dependencies that are no
           longer relevant (see an example below). Asynchronous functions induce
           a partial (rather than a total) order on dependencies, and so [deps]
           should be a linearisation of this partial order. It is also worth
           noting that the problem only occurs with dynamic dependencies,
           because static dependencies can never become irrelevant.

           As an example, consider the function [let f x = let y = g x in h y].
           The correct order of dependencies of [f 0] is [g 0] and then [h y1],
           where [y1] is the result of computing [g 0] in the first build run.
           Now consider the situation where (i) [h y1] is incorrectly listed
           first in [deps], and (ii) both [g] and [h] have changed in the second
           build run (e.g. because they read modified files). To determine that
           [f] needs to be recomputed, we start by recomputing [h y1], which is
           likely to be a waste because now we are really interested in [h y2],
           where [y2] is the result of computing [g 0] in the second run. Had we
           listed [g 0] first, we would recompute it and the work wouldn't be
           wasted since [f 0] does depend on it.

           Another important reason to list [deps] according to a linearisation
           of the dependency order is to eliminate spurious dependency
           cycles. *)
        mutable deps : Dep_node.packed Deps.t
      }
  end =
    Cached_value

  (** The following state transition diagram shows how a node's state changes
      during a build run. After a run completes, every node is guaranteed to end
      up in one of these two states.

      - [Out_of_date]: the node is known to be out of date; it wasn't computed
        during the run, because it is unreachable.

      - [Cached_value]: if the cached value's [run] is current then the node is
        up to date; otherwise, it wasn't restored during the run, because it is
        unreachable.

      {v
            ┌─────────────┐    ┌───────────┐    ┌──────────────┐
            │ Out_of_date ├────► Computing ├────► Cached_value │
            └──────▲──────┘    └───────────┘    └────┬───▲─────┘
                   │                                 │   │
                   │                            ┌────▼───┴─────┐
                   └────────────────────────────┤   Restoring  │
                                                └──────────────┘
      v}

      Between runs, there can be additional state transitions; for example, we
      can invalidate a node by setting its state to [Out_of_date]. *)
  and State : sig
    type 'a t =
      | Cached_value of 'a Cached_value.t
      | Out_of_date of { old_value : 'a Cached_value.t Option.Unboxed.t }
      | Restoring of { restore_from_cache : 'a Cache_lookup.t Computation0.t }
      | Computing of
          { old_value : 'a Cached_value.t Option.Unboxed.t
          ; compute : 'a Cached_value.t Computation0.t
          }
  end =
    State

  and Dep_node : sig
    type ('i, 'o) t =
      { id : Id.t
        (* If [id] is placed first in this data structure, then polymorphic
             comparison for dep nodes works fine regardless of the other fields.
             At the moment polymorphic comparison is used for [Exn_set], but we
             hope to change that. *)
      ; spec : ('i, 'o) Spec.t
      ; input : 'i
      ; mutable state : 'o State.t
      }

    type packed = T : (_, _) t -> packed [@@unboxed]
  end =
    Dep_node

  and Cycle_error : sig
    type t = Dep_node.packed list

    exception E of t
  end = struct
    type t = Dep_node.packed list

    exception E of t
  end

  and Error : sig
    type t =
      { exn : exn
      ; rev_stack : Dep_node.packed list
      }

    exception E of t
  end = struct
    type t =
      { exn : exn
      ; rev_stack : Dep_node.packed list
      }

    exception E of t
  end

  and Cache_lookup : sig
    (* Looking up a value cached in a previous run can fail in three possible ways:

       - [Out_of_date {old_value = None}]: either the value has never been computed
         before, or the last computation attempt failed.

       - [Out_of_date {old_value = Some _}]: we found a value computed in a previous run
         but it is out of date because one of its dependencies changed; we return the old
         value so that it can be compared with a new one to support the early cutoff.

       - [Cancelled _]: the cache lookup attempt has been cancelled because of a
         dependency cycle. This outcome indicates that a dependency cycle has been
         introduced in the current run. If a cycle existed in a previous run, the
         outcome would have been [Out_of_date {old_value = None}] instead. *)
    type 'a t =
      | Ok of 'a Cached_value.t
      | Out_of_date of { old_value : 'a Cached_value.t Option.Unboxed.t }
      | Cancelled of { dependency_cycle : Cycle_error.t }
  end =
    Cache_lookup

  and Dag : (Import.Dag.S with type value := Dep_node.packed) =
    Import.Dag.Make
      (struct
        type t = Dep_node.packed
      end)
      ()

  (* This is similar to [type t = Dag.node Lazy.t] but avoids creating a closure
     with a [dep_node]; the latter is available when we need to [force] a [t]. *)
  and Lazy_dag_node : sig
    type t = Dag.node Option.Unboxed.t ref
  end =
    Lazy_dag_node

  (* A "computation" is represented by an [ivar], filled when the computation is
     finished, and a [dag_node], used for cycle detection before getting blocked
     on reading the [ivar]. When a run completes, all computations are garbage
     collected because we no longer hold any references to them. *)
  and Computation0 : sig
    type 'a t =
      { ivar : 'a Fiber.Ivar.t
      ; dag_node : Lazy_dag_node.t
      }
  end =
    Computation0
end

module Dep_node = struct
  include M.Dep_node

  let input_to_dyn (type i) (node : (i, _) t) =
    let (module Input : Store_intf.Input with type t = i) = node.spec.input in
    Input.to_dyn node.input
  ;;

  let to_dyn_without_state t =
    Dyn.Tuple
      [ String
          (match t.spec.name with
           | Some name -> name
           | None -> "<unnamed>")
      ; input_to_dyn t
      ]
  ;;

  module Packed = struct
    type t = packed

    let id (T t) = t.id
    let equal (T a) (T b) = Id.equal a.id b.id
    let to_dyn_without_state (T t) = to_dyn_without_state t

    let as_instance_of (type i) (T t) (witness : i Type_eq.Id.t) : i option =
      match Type_eq.Id.same witness t.spec.witness with
      | Some Type_eq.T -> Some t.input
      | None -> None
    ;;

    let human_readable_description (T t) =
      Option.map t.spec.human_readable_description ~f:(fun f -> f t.input)
    ;;
  end
end

module Cycle_error = struct
  include M.Cycle_error

  let get t = t
  let to_dyn = Dyn.list Dep_node.Packed.to_dyn_without_state
end

module Error = struct
  include M.Error

  let rotate_cycle ~is_desired_head cycle =
    match List.split_while cycle ~f:(fun elem -> not (is_desired_head elem)) with
    | _, [] -> None
    | prefix, suffix -> Some (suffix @ prefix)
  ;;

  let shorten_stack_leading_to_cycle ~rev_stack cycle =
    let ids_in_cycle = List.map cycle ~f:Dep_node.Packed.id |> Id.Set.of_list in
    match
      List.split_while
        ~f:(fun frame -> not (Id.Set.mem ids_in_cycle (Dep_node.Packed.id frame)))
        rev_stack
    with
    | rev_stack, [] -> rev_stack, cycle
    | rev_stack, node_in_cycle :: _ ->
      let cycle =
        rotate_cycle
          ~is_desired_head:(Dep_node.Packed.equal node_in_cycle)
          (List.rev cycle)
        |> Option.value_exn
        |> List.rev
      in
      rev_stack, cycle
  ;;

  let get_exn_and_stack t =
    match t.exn with
    | Cycle_error.E cycle ->
      let rev_stack, cycle =
        shorten_stack_leading_to_cycle ~rev_stack:t.rev_stack cycle
      in
      Cycle_error.E cycle, List.rev rev_stack
    | exn -> exn, List.rev t.rev_stack
  ;;

  let get t = fst (get_exn_and_stack t)
  let stack t = snd (get_exn_and_stack t)

  let extend_stack exn ~stack_frame =
    E
      (match exn with
       | E t -> { t with rev_stack = stack_frame :: t.rev_stack }
       | _ -> { exn; rev_stack = [ stack_frame ] })
  ;;

  let to_dyn t =
    let open Dyn in
    record
      [ "exn", Exn.to_dyn t.exn
      ; "stack", Dyn.list Dep_node.Packed.to_dyn_without_state (stack t)
      ]
  ;;
end

(* Now that [Error] is defined, teach [Value]'s error-set comparison to unwrap [Error.E]
   (see the comment on [Value.unwrap_exn]). *)
let () =
  Value.unwrap_exn
  := function
     | Error.E { exn; _ } -> exn
     | exn -> exn
;;

module Exn_set = Value.Exn_set
module Collect_errors_monoid = Value.Collect_errors_monoid
module Cache_lookup = M.Cache_lookup
module Dag = M.Dag

(* This is similar to [type t = Dag.node Lazy.t] but avoids creating a closure
   with a [dep_node]; the latter is available when we need to [force] a [t]. *)
module Lazy_dag_node = struct
  include M.Lazy_dag_node

  let create () = ref Option.Unboxed.none

  let force t ~(dep_node : Dep_node.Packed.t) =
    Option.Unboxed.match_
      !t
      ~some:(fun (dag_node : Dag.node) ->
        let dep_node_passed_first = Dag.value dag_node in
        (* CR-someday amokhov: It would be great to restructure the code to rule
           out the potential inconsistency between [dep_node]s passed to [force]. *)
        assert (Dep_node.Packed.equal dep_node dep_node_passed_first);
        dag_node)
      ~none:(fun () ->
        let (dag_node : Dag.node) =
          Counter.incr Metrics.Cycle_detection.nodes;
          Dag.create_node dep_node
        in
        t := Option.Unboxed.some dag_node;
        dag_node)
  ;;
end

module Computation0 = struct
  include M.Computation0

  let create () = { ivar = Fiber.Ivar.create (); dag_node = Lazy_dag_node.create () }
end

(* For debugging *)
let _print_dep_node ?prefix (dep_node : _ Dep_node.t) =
  let prefix =
    match prefix with
    | None -> ""
    | Some prefix -> prefix ^ " "
  in
  (* Printing via [Console.print] leads to incorrectly interleaving messages
     with the diagnostic [printf] output in [memoize_tests]. *)
  if !Debug.verbose_diagnostics
  then
    Format.printf "%s%s\n" prefix (Dep_node.to_dyn_without_state dep_node |> Dyn.to_string)
;;

let get_cached_value_in_current_run (dep_node : _ Dep_node.t) =
  match dep_node.state with
  | Cached_value cv ->
    if Run.is_current (Run.Pair.last_validated_at cv.runs) then Some cv else None
  | Out_of_date _ | Restoring _ | Computing _ -> None
;;

module Cached_value = struct
  include M.Cached_value

  let last_changed_at t = Run.Pair.last_changed_at t.runs
  let last_validated_at t = Run.Pair.last_validated_at t.runs

  let capture_deps ~deps =
    if !Debug.check_invariants
    then
      List.iter (Deps.For_debugging.to_list deps) ~f:(function Dep_node.T dep_node ->
          (match get_cached_value_in_current_run dep_node with
           | None ->
             let reason =
               match dep_node.state with
               | Out_of_date _ -> "(out of date)"
               | Cached_value _ -> "(old run)"
               | Restoring _ -> "(restoring)"
               | Computing _ -> "(computing)"
             in
             Code_error.raise
               ("Attempted to create a cached value based on some stale inputs " ^ reason)
               []
           | Some _up_to_date_cached_value -> ()));
    deps
  ;;

  let create x ~deps =
    let now = Run.current () in
    { value = x
    ; runs = Run.Pair.create ~last_changed_at:now ~last_validated_at:now
    ; deps = capture_deps ~deps
    }
  ;;

  let create_cancelled ~dependency_cycle =
    let now = Run.current () in
    { value =
        Error
          { Collect_errors_monoid.exns =
              Exn_set.singleton
                (Exn_with_backtrace.capture (Cycle_error.E dependency_cycle))
          ; reproducible = false
          }
    ; runs = Run.Pair.create ~last_changed_at:now ~last_validated_at:now
    ; deps =
        (* Dependencies of cancelled computations are not accurate, so we store
           [Deps.empty] in this case. *)
        Deps.empty
    }
  ;;

  let confirm_old_value t ~deps =
    t.runs <- Run.Pair.with_last_validated_at t.runs ~last_validated_at:(Run.current ());
    t.deps <- capture_deps ~deps;
    t
  ;;

  let value_changed (node : _ Dep_node.t) prev_value cur_value =
    match (prev_value : _ Value.t), (cur_value : _ Value.t) with
    | Error { reproducible = false; _ }, _
    | _, Error { reproducible = false; _ }
    | Error _, Ok _
    | Ok _, Error _ -> true
    | Ok prev_value, Ok cur_value ->
      Spec.output_changed node.spec ~old_value:prev_value ~new_value:cur_value
    | ( Error { exns = prev_exns; reproducible = true }
      , Error { exns = cur_exns; reproducible = true } ) ->
      not (Exn_set.equal prev_exns cur_exns)
  ;;

  let to_dyn { value = _; runs; deps = _ } =
    Dyn.record
      [ "value", Opaque
      ; "last_changed_at", Run.to_dyn (Run.Pair.last_changed_at runs)
      ; "last_validated_at", Run.to_dyn (Run.Pair.last_validated_at runs)
      ; "deps", Opaque
      ]
  ;;
end

module State = struct
  include M.State

  let to_dyn = function
    | Cached_value cached_value ->
      Dyn.variant "Cached_value" [ Cached_value.to_dyn cached_value ]
    | Restoring _ -> Dyn.variant "Restoring" [ Opaque ]
    | Computing _ -> Dyn.variant "Computing" [ Opaque ]
    | Out_of_date { old_value } ->
      Dyn.variant
        "Out_of_date"
        [ Dyn.record [ "old_value", Option.Unboxed.to_dyn Cached_value.to_dyn old_value ]
        ]
  ;;
end

module Table = struct
  type ('input, 'output) t =
    { spec : ('input, 'output) Spec.t
    ; cache : ('input, ('input, 'output) Dep_node.t) Store.t
    }
end

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
