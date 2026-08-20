module Metrics0 = Metrics
open! Import
module Metrics = Metrics0
module Console = Console
module Debug = Memo_debug
module Event = Spec.Event
include Fiber
open Fiber.O
module Id = Id.Make ()

module Job_priority_state = struct
  module Demand_class = struct
    type t =
      | Bulk
      | Normal
      | Direct

    let equal a b =
      match a, b with
      | Bulk, Bulk | Normal, Normal | Direct, Direct -> true
      | Bulk, (Normal | Direct) | Normal, (Bulk | Direct) | Direct, (Bulk | Normal) ->
        false
    ;;
  end

  module Root_id = Stdune.Id.Make ()

  type factory =
    { id : Id.t
    ; create : priority:int -> Fiber.Throttle.priority
    }

  type root =
    { id : Root_id.t
    ; demand_class : Demand_class.t
    }

  type t =
    { factory_id : Id.t
    ; priority : Fiber.Throttle.priority
    }

  let current_factory : factory option Fiber.Var.t = Fiber.Var.create None
  let current_root : root option Fiber.Var.t = Fiber.Var.create None
end

module M = struct
  module Import = struct
    module Dag = Dag
  end

  (* A node's lifecycle state. The node's [value], [runs] and [deps] are stored
     flat on the node (see [Dep_node.t]) rather than inside the state, so a node
     keeps its value across state transitions (e.g. an invalidated node keeps its
     old value for the early cutoff check).

     [Not_cached] is the state of a freshly created (never computed) node or a
     node that has been invalidated; [Out_of_date] marks a node found stale
     while restoring it. Both lead to a recomputation, but only [Not_cached]
     fires a [Live] event on recompute (an [Out_of_date] node already fired
     [Live] when it started restoring). *)
  module rec State : sig
    type t =
      | Cached
      | Not_cached
      | Out_of_date
      | Restoring of
          { restore_from_cache : Cycle_error.t Changed_or_not.t Computation0.t }
      | Computing of { compute : unit Computation0.t }
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
      ; mutable state : State.t
      ; mutable value : 'o Value.t
      ; (* We store [last_changed_at] and [last_validated_at] for early cutoff,
             packed into a single immediate [Run.Pair.t] to save memory. See
             Section 5.2.2 of "Build Systems a la Carte: Theory and Practice" for
             more details (https://doi.org/10.1017/S0956796820000088).

             - [last_changed_at] is the run when the value changed last time.

             - [last_validated_at] is the run when the value was last confirmed as
               up to date. Invariant: [last_changed_at <= last_validated_at].

             Consider a dependency [dep] of a node [caller]. If [dep]'s
             [last_changed_at] is greater than [caller]'s [last_validated_at],
             then the [dep]'s value has changed since it had been previously used
             by the [caller] and therefore the [caller] needs to be recomputed. *)
        mutable runs : Run.Pair.t
      ; (* The list of dependencies [deps], as captured at [last_validated_at].
             [deps] should be listed in the order in which they were depended on,
             to avoid recomputing dependencies that are no longer relevant and to
             eliminate spurious dependency cycles. Asynchronous functions induce a
             partial (rather than total) order on dependencies, so [deps] should be
             a linearisation of this partial order. *)
        mutable deps : packed Deps.t
      ; mutable job_priority : int
      ; mutable job_priority_handle : Job_priority_state.t option
      }

    and packed = T : (_, _) t -> packed [@@unboxed]
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
      ; mutable job_priority_dependencies : Dep_node.packed list
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

  let last_changed_at t = Run.Pair.last_changed_at t.runs
  let last_validated_at t = Run.Pair.last_validated_at t.runs

  module Packed = struct
    type t = packed

    let id (T t) = t.id
    let equal (T a) (T b) = Id.equal a.id b.id
    let to_dyn_without_state (T t) = to_dyn_without_state t

    let as_instance_of (type i) (T t) (witness : i Type_eq.Id.t) : i option =
      match t.spec.witness with
      | None -> None
      | Some w ->
        (match Type_eq.Id.same witness w with
         | Some Type_eq.T -> Some t.input
         | None -> None)
    ;;

    let human_readable_description (T t) =
      Option.bind t.spec.human_readable_description ~f:(fun f -> f t.input)
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
module Dag = M.Dag

(* This is similar to [type t = Dag.node Lazy.t] but avoids creating a closure
   with a [dep_node]; the latter is available when we need to [force] a [t]. *)
module Lazy_dag_node = struct
  include M.Lazy_dag_node

  let create () = ref Option.Unboxed.none

  let force t ~(dep_node : Dep_node.Packed.t) =
    Option.Unboxed.match_
      !t
      ~some:(fun (dag_node : Dag.node) -> dag_node)
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

  let create () =
    { ivar = Fiber.Ivar.create ()
    ; dag_node = Lazy_dag_node.create ()
    ; job_priority_dependencies = []
    }
  ;;
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

(* Has the node's [new_value] changed relative to its [old_value], according to
   the cutoff predicate? A fresh ([Uninitialized]) value always counts as
   changed, as does any transition into or out of a non-reproducible error. *)
let value_changed (node : _ Dep_node.t) old_value new_value =
  match (old_value : _ Value.t), (new_value : _ Value.t) with
  | Uninitialized, _ | _, Uninitialized -> true
  | Error { reproducible = false; _ }, _
  | _, Error { reproducible = false; _ }
  | Error _, Ok _
  | Ok _, Error _ -> true
  | Ok old_value, Ok new_value -> Spec.output_changed node.spec ~old_value ~new_value
  | ( Error { exns = prev_exns; reproducible = true }
    , Error { exns = cur_exns; reproducible = true } ) ->
    not (Exn_set.equal prev_exns cur_exns)
;;

(* Mark a node as up to date in the current run: its state becomes [Cached] and
   [last_validated_at] is bumped to the current run. *)
let validate_value (node : _ Dep_node.t) =
  node.runs
  <- Run.Pair.with_last_validated_at node.runs ~last_validated_at:(Run.current ());
  node.state <- Cached;
  Spec.notify node.spec node.input Validated
;;

(* Store a freshly computed (or restored) [value] and [deps] on the node and mark
   it validated. If [value] is unchanged according to the cutoff, we keep the old
   [last_changed_at] (early cutoff) but still refresh [deps] and the
   [last_validated_at] timestamp. *)
let update_value (node : _ Dep_node.t) value ~deps =
  if value_changed node node.value value
  then (
    node.value <- value;
    let now = Run.current () in
    (* Bump both timestamps in a single write so the
       [last_changed_at <= last_validated_at] invariant of [Run.Pair.t] is never
       violated. [validate_value] below rewrites [runs] again, but with the same
       [last_validated_at = now], so the result is unchanged. *)
    node.runs <- Run.Pair.create ~last_changed_at:now ~last_validated_at:now);
  node.deps <- deps;
  validate_value node
;;

(* The dependencies of a node, if it is up to date in the current run. *)
let get_cached_deps_in_current_run (node : _ Dep_node.t) =
  match node.state with
  | Cached ->
    if Run.is_current (Dep_node.last_validated_at node) then Some node.deps else None
  | Not_cached | Out_of_date | Restoring _ | Computing _ -> None
;;

module State = struct
  include M.State

  let to_dyn = function
    | Cached -> Dyn.variant "Cached" []
    | Not_cached -> Dyn.variant "Not_cached" []
    | Out_of_date -> Dyn.variant "Out_of_date" []
    | Restoring _ -> Dyn.variant "Restoring" [ Opaque ]
    | Computing _ -> Dyn.variant "Computing" [ Opaque ]
  ;;
end

(* There are two approaches to invalidating memoization nodes. Currently, when a
   node is invalidated by calling [invalidate], only the node itself is
   marked as "changed" (by setting its [state] to [Out_of_date]). Then the whole
   graph is marked as "possibly changed" by calling [Run.restart ()] that makes
   all remaining [last_validated_at : Run.t] values out of date in O(1) time. In
   the next run, the whole graph is traversed from top to bottom to discover
   "actual changes" and recompute all the nodes affected by these changes. One
   disadvantage of this approach is that the whole graph needs to be traversed
   even if only a small part of it depends on the set of invalidated nodes.

   An alternative approach is as follows. When [invalidate] is called,
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
let invalidate (dep_node : _ Dep_node.t) =
  match dep_node.state with
  | Cached | Out_of_date -> dep_node.state <- Not_cached
  | Not_cached -> ()
  | Restoring _ ->
    Code_error.raise
      "Node.invalidate called on a node in Restoring state"
      [ "dep_node", Dep_node.to_dyn_without_state dep_node ]
  | Computing _ ->
    Code_error.raise
      "Node.invalidate called on a node in Computing state"
      [ "dep_node", Dep_node.to_dyn_without_state dep_node ]
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

module Call_stack = struct
  type t = Stack_frame_with_state.t list

  (* The variable holding the call stack for the current context. *)
  let call_stack_var : t Fiber.Var.t = Fiber.Var.create []
  let get_call_stack () = Fiber.Var.get call_stack_var

  let get_call_stack_without_state () =
    get_call_stack () >>| List.map ~f:Stack_frame_with_state.dep_node
  ;;

  let push_frame (frame : Stack_frame_with_state.t) f =
    (* Update the call-stack variable and run [f] (with implicit output forbidden) in a
       single effect, without reading the stack first or allocating a thunk per push. *)
    Fiber.Var.update_apply
      call_stack_var
      ~f:(fun stack -> frame :: stack)
      Implicit_output.forbid
      f
  ;;

  (* As soon as we hit a cycle error in the current run, we stop adding new edges to the
     cycle detection DAG. Calling [Dag.add_assuming_missing] after a cycle has been
     detected is unsafe: it can violate the DAG's internal invariants (and previously
     caused Dune to hang). This holds the first (and only) cycle error we hit in the
     current run; it is reset by [reset]. *)
  let cycle_error_in_the_current_run : Cycle_error.t option ref = ref None
  let reset_cycle_error_in_the_current_run () = cycle_error_in_the_current_run := None

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
end

module Job_priority = struct
  module Demand_class = Job_priority_state.Demand_class
  module Root_id = Job_priority_state.Root_id

  type t = Fiber.Throttle.priority
  type factory = Job_priority_state.factory
  type root = Job_priority_state.root

  let with_factory create f =
    let factory = { Job_priority_state.id = Id.gen (); create } in
    Fiber.Var.set Job_priority_state.current_factory (Some factory) f
  ;;

  let current_factory () = Fiber.Var.get Job_priority_state.current_factory

  let with_root demand_class f =
    let* factory = current_factory () in
    match factory with
    | None -> f ()
    | Some _ ->
      let root = { Job_priority_state.id = Root_id.gen (); demand_class } in
      Fiber.Var.set Job_priority_state.current_root (Some root) f
  ;;

  let current_root () = Fiber.Var.get Job_priority_state.current_root
  let root_id { Job_priority_state.id; demand_class = _ } = id
  let root_demand_class { Job_priority_state.id = _; demand_class } = demand_class

  module For_tests = struct
    let current_root () =
      let+ root = current_root () in
      Option.map root ~f:(fun root ->
        root_demand_class root, Root_id.to_int (root_id root))
    ;;
  end

  let succ priority = if priority = Int.max_int then priority else priority + 1

  let is_active_dependency (Dep_node.T dep_node) =
    match dep_node.state with
    | Computing _ | Restoring _ -> true
    | Cached | Not_cached | Out_of_date -> false
  ;;

  let active_dependencies (dep_node : _ Dep_node.t) =
    let active dependencies = List.filter dependencies ~f:is_active_dependency in
    match dep_node.state with
    | Computing { compute } ->
      let dependencies = active compute.job_priority_dependencies in
      compute.job_priority_dependencies <- dependencies;
      dependencies
    | Restoring { restore_from_cache } ->
      let dependencies = active restore_from_cache.job_priority_dependencies in
      restore_from_cache.job_priority_dependencies <- dependencies;
      dependencies
    | Cached | Not_cached | Out_of_date -> []
  ;;

  let update_priority (dep_node : _ Dep_node.t) ~priority ~(factory : factory) =
    if priority <= dep_node.job_priority
    then false
    else (
      let increment = priority - dep_node.job_priority in
      dep_node.job_priority <- priority;
      (match dep_node.job_priority_handle with
       | Some current when Id.equal current.factory_id factory.id ->
         Fiber.Throttle.increase_priority_by current.priority increment
       | None | Some _ -> ());
      true)
  ;;

  let increase_to_at_least (dependency : Dep_node.packed) ~priority ~(factory : factory) =
    let rec loop = function
      | [] -> ()
      | (Dep_node.T dep_node, priority, visited) :: work ->
        if
          Id.Set.mem visited dep_node.id
          || not (update_priority dep_node ~priority ~factory)
        then loop work
        else (
          match active_dependencies dep_node with
          | [] -> loop work
          | dependencies ->
            let visited = Id.Set.add visited dep_node.id in
            let priority = succ priority in
            let work =
              List.fold_left dependencies ~init:work ~f:(fun work dependency ->
                (dependency, priority, visited) :: work)
            in
            loop work)
    in
    loop [ dependency, priority, Id.Set.empty ]
  ;;

  let increase (dep_node : _ Dep_node.t) ~caller ~(factory : factory option) =
    match factory, caller with
    | None, _ | Some _, None -> ()
    | Some factory, Some caller ->
      let (Dep_node.T caller) = Stack_frame_with_state.dep_node caller in
      let priority = succ (max dep_node.job_priority caller.job_priority) in
      increase_to_at_least (Dep_node.T dep_node) ~priority ~factory
  ;;

  let inherit_from_dependency
        (dep_node : _ Dep_node.t)
        ~caller
        ~(factory : factory option)
    =
    match factory, caller with
    | None, _ | Some _, None -> ()
    | Some factory, Some caller ->
      let (Dep_node.T caller) = Stack_frame_with_state.dep_node caller in
      ignore
        (update_priority
           caller
           ~priority:(max caller.job_priority dep_node.job_priority)
           ~factory
         : bool)
  ;;

  let add_dependency (dep_node : _ Dep_node.t) ~caller =
    match caller with
    | None -> ()
    | Some caller ->
      let (Dep_node.T caller) = Stack_frame_with_state.dep_node caller in
      let dependency = Dep_node.T dep_node in
      (match caller.state with
       | Computing { compute } ->
         compute.job_priority_dependencies
         <- dependency :: compute.job_priority_dependencies
       | Restoring { restore_from_cache } ->
         restore_from_cache.job_priority_dependencies
         <- dependency :: restore_from_cache.job_priority_dependencies
       | Cached | Not_cached | Out_of_date ->
         Code_error.raise
           "Job_priority.add_dependency: caller is not active"
           [ "caller", Dep_node.to_dyn_without_state caller
           ; "state", State.to_dyn caller.state
           ])
  ;;

  let current () =
    let* stack = Call_stack.get_call_stack () in
    let+ factory = Fiber.Var.get Job_priority_state.current_factory in
    match List.hd_opt stack, factory with
    | Some frame, Some factory ->
      let (Dep_node.T dep_node) = Stack_frame_with_state.dep_node frame in
      let priority =
        match dep_node.job_priority_handle with
        | Some current when Id.equal current.factory_id factory.id -> current.priority
        | None | Some _ ->
          let priority = factory.create ~priority:dep_node.job_priority in
          dep_node.job_priority_handle
          <- Some { Job_priority_state.factory_id = factory.id; priority };
          priority
      in
      Some priority
    | None, _ | _, None -> None
  ;;
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
  let force { ivar; dag_node; _ } ~phase ~dep_node fiber =
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

  let read_but_first_check_for_cycles { ivar; dag_node; _ } ~phase ~dep_node =
    match Fiber.Ivar.peek ivar with
    | Some res -> Fiber.return (Ok res)
    | None ->
      (match (phase : Stack_frame_with_state.phase) with
       | Restore_from_cache -> Counter.incr Metrics.Restore.blocked
       | Compute -> Counter.incr Metrics.Compute.blocked);
      let dag_node = Lazy_dag_node.force dag_node ~dep_node:(Dep_node.T dep_node) in
      Call_stack.add_path_to ~dag_node
      >>= (function
       | Ok () -> Fiber.Ivar.read ivar >>| Result.ok
       | Error _ as cycle_error -> Fiber.return cycle_error)
  ;;
end
