open! Stdune
open Fiber.O
module Graph = Dune_graph.Graph
module Console = Dune_console

module Debug = struct
  let track_locations_of_lazy_values = ref false

  let check_invariants = ref false

  let verbose_diagnostics = ref false
end

module Counters = struct
  let enabled = ref false

  let nodes_restored = ref 0

  let nodes_computed = ref 0

  let edges_traversed = ref 0

  let nodes_in_cycle_detection_graph = ref 0

  let edges_in_cycle_detection_graph = ref 0

  let paths_in_cycle_detection_graph = ref 0

  let reset () =
    nodes_restored := 0;
    nodes_computed := 0;
    edges_traversed := 0;
    nodes_in_cycle_detection_graph := 0;
    edges_in_cycle_detection_graph := 0;
    paths_in_cycle_detection_graph := 0
end

include Fiber

let when_ x y =
  match x with
  | true -> y ()
  | false -> return ()

let of_reproducible_fiber = Fun.id

module Allow_cutoff = struct
  type 'o t =
    | No
    | Yes of ('o -> 'o -> bool)
end

module type Input = sig
  type t

  include Table.Key with type t := t
end

module Spec = struct
  type ('i, 'o) t =
    { name : string option
    ; (* If the field [witness] precedes any of the functional values ([input]
         and [f]), then polymorphic comparison actually works for [Spec.t]s. *)
      witness : 'i Type_eq.Id.t
    ; input : (module Store_intf.Input with type t = 'i)
    ; allow_cutoff : 'o Allow_cutoff.t
    ; f : 'i -> 'o Fiber.t
    ; human_readable_description : ('i -> User_message.Style.t Pp.t) option
    }

  let create ~name ~input ~human_readable_description ~cutoff f =
    let name =
      match name with
      | None when !Debug.track_locations_of_lazy_values ->
        Option.map (Caller_id.get ~skip:[ __FILE__ ]) ~f:(fun loc ->
            sprintf "lazy value created at %s" (Loc.to_file_colon_line loc))
      | _ -> name
    in
    let allow_cutoff =
      match cutoff with
      | None -> Allow_cutoff.No
      | Some equal -> Yes equal
    in
    { name
    ; input
    ; allow_cutoff
    ; witness = Type_eq.Id.create ()
    ; f
    ; human_readable_description
    }
end

module Id = Id.Make ()

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)
module Caches = struct
  let cleaners = ref []

  let register ~clear = cleaners := clear :: !cleaners

  let clear () = List.iter !cleaners ~f:(fun f -> f ())
end

module Dep_node_without_state = struct
  type ('i, 'o) t =
    { id : Id.t
          (* If [id] is placed first in this data structure, then polymorphic
             comparison for dep nodes works fine regardless of the other fields.
             At the moment polymorphic comparison is used for [Exn_set], but we
             hope to change that. *)
    ; spec : ('i, 'o) Spec.t
    ; input : 'i
    }

  type packed = T : (_, _) t -> packed [@@unboxed]

  let input_to_dyn (type i) (node : (i, _) t) =
    let (module Input : Store_intf.Input with type t = i) = node.spec.input in
    Input.to_dyn node.input

  let to_dyn t =
    Dyn.Tuple
      [ String
          (match t.spec.name with
          | Some name -> name
          | None -> "<unnamed>")
      ; input_to_dyn t
      ]
end

module Stack_frame_without_state = struct
  open Dep_node_without_state

  type t = Dep_node_without_state.packed

  let name (T t) = t.spec.name

  let input (T t) = input_to_dyn t

  let to_dyn (T t) = Dep_node_without_state.to_dyn t

  let id (T a) = a.id

  let equal (T a) (T b) = Id.equal a.id b.id
end

module Cycle_error = struct
  type t = Stack_frame_without_state.t list

  exception E of t

  let get t = t

  let to_dyn = Dyn.list Stack_frame_without_state.to_dyn
end

module Error = struct
  type t =
    { exn : exn
    ; rev_stack : Stack_frame_without_state.t list
    }

  exception E of t

  let rotate_cycle ~is_desired_head cycle =
    match
      List.split_while cycle ~f:(fun elem -> not (is_desired_head elem))
    with
    | _, [] -> None
    | prefix, suffix -> Some (suffix @ prefix)

  let shorten_stack_leading_to_cycle ~rev_stack cycle =
    let ids_in_cycle =
      List.map cycle ~f:Stack_frame_without_state.id |> Id.Set.of_list
    in
    match
      List.split_while
        ~f:(fun frame ->
          not (Id.Set.mem ids_in_cycle (Stack_frame_without_state.id frame)))
        rev_stack
    with
    | rev_stack, [] -> (rev_stack, cycle)
    | rev_stack, node_in_cycle :: _ ->
      let cycle =
        rotate_cycle
          ~is_desired_head:(Stack_frame_without_state.equal node_in_cycle)
          (List.rev cycle)
        |> Option.value_exn |> List.rev
      in
      (rev_stack, cycle)

  let get_exn_and_stack t =
    match t.exn with
    | Cycle_error.E cycle ->
      let rev_stack, cycle =
        shorten_stack_leading_to_cycle ~rev_stack:t.rev_stack cycle
      in
      (Cycle_error.E cycle, List.rev rev_stack)
    | exn -> (exn, List.rev t.rev_stack)

  let get t = fst (get_exn_and_stack t)

  let stack t = snd (get_exn_and_stack t)

  let extend_stack exn ~stack_frame =
    E
      (match exn with
      | E t -> { t with rev_stack = stack_frame :: t.rev_stack }
      | _ -> { exn; rev_stack = [ stack_frame ] })

  let to_dyn t =
    let open Dyn in
    record
      [ ("exn", Exn.to_dyn t.exn)
      ; ("stack", Dyn.list Stack_frame_without_state.to_dyn (stack t))
      ]
end

(* The user can wrap exceptions into the [Non_reproducible] constructor to tell
   Memo that they shouldn't be cached. We will catch them, unwrap, and re-raise
   without the wrapper. *)
exception Non_reproducible of exn

let () =
  Printexc.register_printer (fun exn ->
      let dyn =
        let open Dyn in
        match exn with
        | Error.E err -> Some (variant "Memo.Error.E" [ Error.to_dyn err ])
        | Cycle_error.E frames ->
          Some (variant "Cycle_error.E" [ Cycle_error.to_dyn frames ])
        | Non_reproducible exn ->
          Some (variant "Memo.Non_reproducible" [ Exn.to_dyn exn ])
        | _ -> None
      in
      Option.map dyn ~f:Dyn.to_string)

module Exn_comparable = Comparable.Make (struct
  type t = Exn_with_backtrace.t

  let unwrap = function
    | Error.E { exn; _ } -> exn
    | exn -> exn

  let compare { Exn_with_backtrace.exn; backtrace = _ } (t : t) =
    Poly.compare (unwrap exn) (unwrap t.exn)

  let to_dyn = Exn_with_backtrace.to_dyn
end)

module Exn_set = Exn_comparable.Set

module Collect_errors_monoid = struct
  module T = struct
    type t =
      { exns : Exn_set.t
      ; reproducible : bool
      }

    let empty = { exns = Exn_set.empty; reproducible = true }

    let combine { exns = exns1; reproducible = reproducible1 }
        { exns = exns2; reproducible = reproducible2 } =
      { exns = Exn_set.union exns1 exns2
      ; reproducible = reproducible1 && reproducible2
      }
  end

  include T
  include Monoid.Make (T)
end

(* Restoring or computing a value can fail for two reasons:

   - [Error]: the user-supplied function that was called to compute the value
   raised one or more exceptions recorded in the [Collect_errors_monoid.t].

   - [Cancelled]: the attempt was cancelled due to a dependency cycle.

   Note that we plan to make [Cancelled] more general and store the reason for
   cancellation: a dependency cycle or a request to cancel the current run. *)
module Value = struct
  type 'a t =
    | Ok of 'a
    | Error of Collect_errors_monoid.t
    | Cancelled of { dependency_cycle : Cycle_error.t }

  let get_exn t ~stack_frame =
    match t with
    | Ok a -> Fiber.return a
    | Error { exns; _ } ->
      Fiber.reraise_all
        (Exn_set.to_list_map exns ~f:(fun exn ->
             { exn with exn = Error.extend_stack exn.exn ~stack_frame }))
    | Cancelled { dependency_cycle } -> raise (Cycle_error.E dependency_cycle)
end

module Cache_lookup = struct
  (* Looking up a value cached in a previous run can fail in three possible
     ways:

     - [Out_of_date {old_value = None}]: either the value has never been
     computed before, or the last computation attempt failed.

     - [Out_of_date {old_value = Some _}]: we found a value computed in a
     previous run but it is out of date because one of its dependencies changed;
     we return the old value so that it can be compared with a new one to
     support the early cutoff.

     - [Cancelled _]: the cache lookup attempt has been cancelled because of a
     dependency cycle. This outcome indicates that a dependency cycle has been
     introduced in the current run. If a cycle existed in a previous run, the
     outcome would have been [Out_of_date {old_value = None}] instead. *)
  module Failure = struct
    type 'a t =
      | Out_of_date of { old_value : 'a option }
      | Cancelled of { dependency_cycle : Cycle_error.t }
  end

  module Result = struct
    type 'a t =
      | Ok of 'a
      | Failure of 'a Failure.t

    let _to_string_hum = function
      | Ok _ -> "Ok"
      | Failure (Out_of_date { old_value = None }) -> "Failed (no value)"
      | Failure (Out_of_date { old_value = Some _ }) -> "Failed (old value)"
      | Failure (Cancelled _) -> "Failed (dependency cycle)"
  end
end

module Dag : Dag.S with type value := Dep_node_without_state.packed =
  Dag.Make
    (struct
      type t = Dep_node_without_state.packed
    end)
    ()

(* This is similar to [type t = Dag.node Lazy.t] but avoids creating a closure
   with a [dep_node]; the latter is available when we need to [force] a [t]. *)
module Lazy_dag_node = struct
  type t = Dag.node option ref

  let create () = ref None

  let force t ~(dep_node : _ Dep_node_without_state.t) =
    match !t with
    | Some (({ data = T dep_node_passed_first; _ } : Dag.node) as dag_node) ->
      (* CR-someday amokhov: It would be great to restructure the code to rule
         out the potential inconsistency between [dep_node]s passed to
         [force]. *)
      assert (Id.equal dep_node.id dep_node_passed_first.id);
      dag_node
    | None ->
      let (dag_node : Dag.node) =
        if !Counters.enabled then incr Counters.nodes_in_cycle_detection_graph;
        { info = Dag.create_node_info ()
        ; data = Dep_node_without_state.T dep_node
        }
      in
      t := Some dag_node;
      dag_node
end

(* A "computation" is represented by an [ivar], filled when the computation is
   finished, and a [dag_node], used for cycle detection before getting blocked
   on reading the [ivar]. When a run completes, all computations are garbage
   collected because we no longer hold any references to them. *)
module Computation0 = struct
  type 'a t =
    { ivar : 'a Fiber.Ivar.t
    ; dag_node : Lazy_dag_node.t
    }

  let create () =
    { ivar = Fiber.Ivar.create (); dag_node = Lazy_dag_node.create () }
end

(* Checking dependencies of a node can lead to one of these outcomes:

   - [Unchanged]: all the dependencies of the current node are up to date and we
   can therefore skip recomputing the node and can reuse the value computed in
   the previous run.

   - [Changed]: one of the dependencies has changed since the previous run and
   the current node should therefore be recomputed.

   - [Cancelled _]: one of the dependencies leads to a dependency cycle. In this
   case, there is no point in recomputing the current node: it's impossible to
   bring its dependencies up to date! *)
module Changed_or_not = struct
  type t =
    | Unchanged
    | Changed
    | Cancelled of { dependency_cycle : Cycle_error.t }
end

module M = struct
  (* A [value] along with some additional information that allows us to check
     whether the it is up to date or needs to be recomputed. *)
  module rec Cached_value : sig
    type 'a t =
      { value : 'a Value.t
      ; (* We store [last_changed_at] and [last_validated_at] for early cutoff.
           See Section 5.2.2 of "Build Systems a la Carte: Theory and Practice"
           for more details (https://doi.org/10.1017/S0956796820000088).

           - [last_changed_at] is the run when the value changed last time.

           - [last_validated_at] is the run when the value was last confirmed as
           up to date. Invariant: [last_changed_at <= last_validated_at].

           Consider a dependency [dep] of a node [caller].

           If [dep.last_changed_at > caller.last_validated_at], then the [dep]'s
           value has changed since it had been previously used by the [caller]
           and therefore the [caller] needs to be recomputed. *)
        last_changed_at : Run.t
      ; mutable last_validated_at : Run.t
      ; (* The list of dependencies [deps], as captured at [last_validated_at].
           Note that the list of dependencies can change over the lifetime of
           [Cached_value]: this happens if the value gets re-computed but is
           declared unchanged by the cutoff check.

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
        mutable deps : Deps.t
      }
  end =
    Cached_value

  and Deps : sig
    type t

    val create : deps_rev:Dep_node.packed list -> t

    val empty : t

    val length : t -> int

    val to_list : t -> Dep_node.packed list

    val changed_or_not :
         t
      -> f:(Dep_node.packed -> Changed_or_not.t Fiber.t)
      -> Changed_or_not.t Fiber.t
  end = struct
    (* The array is stored reversed to avoid reversing the list in [create]. We
       need to be careful about traversing the array in the right order in the
       functions [to_list] and [changed_or_not]. *)
    type t = Dep_node.packed array

    let create ~deps_rev = Array.of_list deps_rev

    let empty = Array.init 0 ~f:(fun _ -> assert false)

    let length = Array.length

    let to_list = Array.fold_left ~init:[] ~f:(fun acc x -> x :: acc)

    let changed_or_not t ~f =
      let rec go index =
        if index < 0 then (
          if !Counters.enabled then
            Counters.edges_traversed :=
              !Counters.edges_traversed + Array.length t;
          Fiber.return Changed_or_not.Unchanged)
        else
          f t.(index) >>= function
          | Changed_or_not.Unchanged -> go (index - 1)
          | (Changed | Cancelled _) as res ->
            if !Counters.enabled then
              Counters.edges_traversed :=
                !Counters.edges_traversed + (Array.length t - index);
            Fiber.return res
      in
      go (Array.length t - 1)
  end

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
      | Out_of_date of { old_value : 'a Cached_value.t option }
      | Restoring of
          { restore_from_cache :
              'a Cached_value.t Cache_lookup.Result.t Computation0.t
          }
      | Computing of
          { old_value : 'a Cached_value.t option
          ; compute : 'a Cached_value.t Computation0.t
          }
  end =
    State

  and Dep_node : sig
    type ('i, 'o) t =
      { without_state : ('i, 'o) Dep_node_without_state.t
      ; mutable state : 'o State.t
      ; (* This field caches the value of [without_state.spec.allow_cutoff] to
           avoid jumping through two more pointers in a tight loop. *)
        has_cutoff : bool
      }

    type packed = T : (_, _) t -> packed [@@unboxed]
  end =
    Dep_node
end

module Dep_node = M.Dep_node
module Deps = M.Deps

(* For debugging *)
let _print_dep_node ?prefix (dep_node : _ Dep_node.t) =
  let prefix =
    match prefix with
    | None -> ""
    | Some prefix -> prefix ^ " "
  in
  (* Printing via [Console.print] leads to incorrectly interleaving messages
     with the diagnostic [printf] output in [memoize_tests]. *)
  if !Debug.verbose_diagnostics then
    Format.printf "%s%s\n" prefix
      (Dep_node_without_state.to_dyn dep_node.without_state |> Dyn.to_string)

module Stack_frame_with_state : sig
  type phase =
    | Restore_from_cache
    | Compute

  type t

  val to_dyn : t -> Dyn.t

  (* Create a new stack frame related to restoring or computing a [dep_node]. *)
  val create :
       dag_node:Lazy_dag_node.t
    -> phase
    -> dep_node:_ Dep_node_without_state.t
    -> t

  val dep_node : t -> Dep_node_without_state.packed

  val dag_node : t -> Dag.node

  (* This function accumulates dependencies of frames in the [Compute] phase.
     Calling it in the [Restore_from_cache] frame raises an exception. *)
  val add_dep : t -> dep_node:_ Dep_node.t -> unit

  val deps_rev : t -> Dep_node.packed list

  val children_added_to_dag : t -> Dag.Id.Set.t

  val record_child_added_to_dag : t -> dag_node_id:Dag.Id.t -> unit
end = struct
  type phase =
    | Restore_from_cache
    | Compute

  type ('i, 'o) unpacked =
    { dep_node : ('i, 'o) Dep_node_without_state.t
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
    ; (* The only purpose of storing [phase] is to ensure (via an [assert]) that
         we are accumulating dependencies only during the [Compute] phase. We
         can drop it if we find a way to statically guarantee this property. *)
      phase : phase
    ; (* [deps_rev] are accumulated only when [phase = Compute], see
         [add_dep]. *)
      mutable deps_rev : Dep_node.packed list
    }

  type t = T : ('i, 'o) unpacked -> t [@@unboxed]

  let to_dyn (T t) = Stack_frame_without_state.to_dyn (T t.dep_node)

  let create ~dag_node phase ~dep_node =
    T
      { dep_node
      ; phase
      ; deps_rev = []
      ; dag_node
      ; children_added_to_dag = Dag.Id.Set.empty
      }

  let dep_node (T t) = Dep_node_without_state.T t.dep_node

  let dag_node (T t) = Lazy_dag_node.force t.dag_node ~dep_node:t.dep_node

  let add_dep (T t) ~dep_node =
    match t.phase with
    | Restore_from_cache -> assert false
    | Compute -> t.deps_rev <- Dep_node.T dep_node :: t.deps_rev

  let deps_rev (T t) = t.deps_rev

  let record_child_added_to_dag (T t) ~dag_node_id =
    t.children_added_to_dag <-
      Dag.Id.Set.add t.children_added_to_dag dag_node_id

  let children_added_to_dag (T t) = t.children_added_to_dag
end

module To_open = struct
  module Stack_frame = Stack_frame_with_state
end

open To_open

module Call_stack = struct
  type t = Stack_frame_with_state.t list

  (* The variable holding the call stack for the current context. *)
  let call_stack_var : t Fiber.Var.t = Fiber.Var.create ()

  let get_call_stack () =
    Fiber.Var.get call_stack_var >>| Option.value ~default:[]

  let get_call_stack_without_state () =
    get_call_stack () >>| List.map ~f:Stack_frame_with_state.dep_node

  let push_frame (frame : Stack_frame_with_state.t) f =
    let* stack = get_call_stack () in
    let stack = frame :: stack in
    Fiber.Var.set call_stack_var stack (fun () -> Implicit_output.forbid f)

  (* Add all edges leading from the root of the call stack to [dag_node] to the
     cycle detection DAG. *)
  let add_path_to ~dag_node : (unit, Cycle_error.t) result Fiber.t =
    let+ stack = get_call_stack () in
    let rec add_path_impl stack dag_node =
      match stack with
      | [] -> Ok ()
      | frame :: stack -> (
        let dag_node_id = Dag.node_id dag_node in
        let children_added_to_dag =
          Stack_frame_with_state.children_added_to_dag frame
        in
        match Dag.Id.Set.mem children_added_to_dag dag_node_id with
        | true ->
          (* Here we know that the current [frame] has already been traversed in
             a previous [add_path_to] call. Therefore, the DAG already contains
             all the edges that we will discover by continuing the recursive
             traversal. We might as well stop here and save time. *)
          Ok ()
        | false -> (
          let caller_dag_node = Stack_frame_with_state.dag_node frame in
          match Dag.add_assuming_missing caller_dag_node dag_node with
          | exception Dag.Cycle cycle ->
            Error (List.map cycle ~f:(fun dag_node -> dag_node.Dag.data))
          | () -> (
            if !Counters.enabled then
              incr Counters.edges_in_cycle_detection_graph;
            let not_traversed_before =
              Dag.Id.Set.is_empty children_added_to_dag
            in
            Stack_frame_with_state.record_child_added_to_dag frame ~dag_node_id;
            match not_traversed_before with
            | true -> add_path_impl stack caller_dag_node
            | false ->
              (* Same optimisation as above: no need to traverse again. *)
              Ok ())))
    in
    if !Counters.enabled then incr Counters.paths_in_cycle_detection_graph;
    add_path_impl stack dag_node

  (* Add a dependency on the [dep_node] from the caller, if there is one. *)
  let add_dep_from_caller =
    let get_call_stack_tip () = get_call_stack () >>| List.hd_opt in
    fun dep_node ->
      let+ caller = get_call_stack_tip () in
      match caller with
      | None -> ()
      | Some caller -> Stack_frame_with_state.add_dep caller ~dep_node
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
    let+ () = Fiber.Ivar.fill ivar result in
    result

  let read_but_first_check_for_cycles { ivar; dag_node } ~dep_node =
    Fiber.Ivar.peek ivar >>= function
    | Some res -> Fiber.return (Ok res)
    | None -> (
      let dag_node = Lazy_dag_node.force dag_node ~dep_node in
      Call_stack.add_path_to ~dag_node >>= function
      | Ok () -> Fiber.Ivar.read ivar >>| Result.ok
      | Error _ as cycle_error -> Fiber.return cycle_error)
end

module Error_handler : sig
  val is_set : bool Fiber.t

  val report_error : Exn_with_backtrace.t -> unit Fiber.t

  val with_error_handler :
    (Exn_with_backtrace.t -> unit Fiber.t) -> (unit -> 'a Fiber.t) -> 'a Fiber.t
end = struct
  type t = Exn_with_backtrace.t -> unit Fiber.t

  let var : t Fiber.Var.t = Fiber.Var.create ()

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
          Code_error.raise "Memo error handler raised an exception"
            [ ("exns", Dyn.list Exn_with_backtrace.to_dyn e) ])

  let deduplicate_errors f =
    let reported = ref Exn_set.empty in
    fun exn ->
      if Exn_set.mem !reported exn then Fiber.return ()
      else (
        reported := Exn_set.add !reported exn;
        f exn)

  let with_error_handler t f =
    Fiber.of_thunk (fun () ->
        (* [with_error_handler] runs once for every incremental run, so calling
           [deduplicate_errors] afresh here makes sure that we re-report all
           errors*)
        let t = deduplicate_errors t in
        Fiber.bind (Fiber.Var.get var) ~f:(function
          | None -> Fiber.Var.set var t f
          | Some _handler ->
            Code_error.raise
              "Memo.run_with_error_handler: an error handler is already \
               installed"
              []))
end

let pp_stack () =
  let open Pp.O in
  let+ stack = Call_stack.get_call_stack () in
  Pp.vbox
    (Pp.box (Pp.text "Memoized function stack:")
    ++ Pp.cut
    ++ Pp.chain stack ~f:(fun frame -> Dyn.pp (Stack_frame.to_dyn frame)))

let dump_stack () =
  let+ pp = pp_stack () in
  Console.print [ pp ]

let get_cached_value_in_current_run (dep_node : _ Dep_node.t) =
  match dep_node.state with
  | Cached_value cv ->
    if Run.is_current cv.last_validated_at then Some cv else None
  | Out_of_date _ | Restoring _ | Computing _ -> None

module Cached_value = struct
  include M.Cached_value

  let capture_deps ~deps_rev =
    if !Debug.check_invariants then
      List.iter deps_rev ~f:(function Dep_node.T dep_node ->
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
              ("Attempted to create a cached value based on some stale inputs "
             ^ reason)
              []
          | Some _up_to_date_cached_value -> ()));
    Deps.create ~deps_rev

  let create x ~deps_rev =
    { value = x
    ; last_changed_at = Run.current ()
    ; last_validated_at = Run.current ()
    ; deps = capture_deps ~deps_rev
    }

  (* Dependencies of cancelled computations are not accurate, so we store the
     empty list of [deps] in this case. In future, it would be better to
     refactor the code to avoid storing the list altogether in this case. *)
  let create_cancelled ~dependency_cycle =
    { value = Cancelled { dependency_cycle }
    ; last_changed_at = Run.current ()
    ; last_validated_at = Run.current ()
    ; deps = Deps.empty
    }

  let confirm_old_value t ~deps_rev =
    t.last_validated_at <- Run.current ();
    t.deps <- capture_deps ~deps_rev;
    t

  let value_changed (node : _ Dep_node.t) prev_value cur_value =
    match ((prev_value : _ Value.t), (cur_value : _ Value.t)) with
    | (Cancelled _ | Error { reproducible = false; _ }), _
    | _, (Cancelled _ | Error { reproducible = false; _ })
    | Error _, Ok _
    | Ok _, Error _ -> true
    | Ok prev_value, Ok cur_value -> (
      match node.without_state.spec.allow_cutoff with
      | Yes equal -> not (equal prev_value cur_value)
      | No -> true)
    | ( Error { exns = prev_exns; reproducible = true }
      , Error { exns = cur_exns; reproducible = true } ) ->
      not (Exn_set.equal prev_exns cur_exns)

  let to_dyn { value = _; last_changed_at; last_validated_at; deps = _ } =
    Dyn.record
      [ ("value", Opaque)
      ; ("last_changed_at", Run.to_dyn last_changed_at)
      ; ("last_validated_at", Run.to_dyn last_validated_at)
      ; ("deps", Opaque)
      ]
end

module State = struct
  include M.State

  let to_dyn = function
    | Cached_value cached_value ->
      Dyn.variant "Cached_value" [ Cached_value.to_dyn cached_value ]
    | Restoring _ -> Dyn.variant "Restoring" [ Opaque ]
    | Computing _ -> Dyn.variant "Computing" [ Opaque ]
    | Out_of_date { old_value } ->
      Dyn.variant "Out_of_date"
        [ Dyn.record [ ("old_value", Dyn.option Cached_value.to_dyn old_value) ]
        ]
end

module Table = struct
  type ('input, 'output) t =
    { spec : ('input, 'output) Spec.t
    ; cache : ('input, ('input, 'output) Dep_node.t) Store.t
    }
end

module Stack_frame = struct
  include Stack_frame_without_state

  let as_instance_of (type i) (Dep_node_without_state.T t)
      ~of_:(memo : (i, _) Table.t) : i option =
    match Type_eq.Id.same memo.spec.witness t.spec.witness with
    | Some Type_eq.T -> Some t.input
    | None -> None

  let human_readable_description (Dep_node_without_state.T t) =
    Option.map t.spec.human_readable_description ~f:(fun f -> f t.input)
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
    dep_node.state <- Out_of_date { old_value = Some cached_value }
  | Out_of_date { old_value = _ } -> ()
  | Restoring _ ->
    Code_error.raise "invalidate_dep_node called on a node in Restoring state"
      [ ("dep_node", Dep_node_without_state.to_dyn dep_node.without_state) ]
  | Computing _ ->
    Code_error.raise "invalidate_dep_node called on a node in Computing state"
      [ ("dep_node", Dep_node_without_state.to_dyn dep_node.without_state) ]

let invalidate_store = Store.iter ~f:invalidate_dep_node

let create_with_cache (type i o) name ~cache ~input ~cutoff
    ~human_readable_description (f : i -> o Fiber.t) : (i, o) Table.t =
  let spec =
    Spec.create ~name:(Some name) ~input ~cutoff ~human_readable_description f
  in
  Caches.register ~clear:(fun () ->
      Store.clear cache;
      invalidate_store cache);
  { cache; spec }

let create_with_store (type i) name
    ~store:(module S : Store_intf.S with type key = i) ~input ?cutoff
    ?human_readable_description f =
  let cache = Store.make (module S) in
  create_with_cache name ~cache ~input ~cutoff ~human_readable_description f

let create (type i) name ~input:(module Input : Input with type t = i) ?cutoff
    ?human_readable_description f =
  (* This mutable table is safe: the implementation tracks all dependencies. *)
  let cache = Store.of_table (Stdune.Table.create (module Input) 2) in
  let input = (module Input : Store_intf.Input with type t = i) in
  create_with_cache name ~cache ~input ~cutoff ~human_readable_description f

let make_dep_node ~spec ~input : _ Dep_node.t =
  let dep_node_without_state : _ Dep_node_without_state.t =
    { id = Id.gen (); input; spec }
  in
  { without_state = dep_node_without_state
  ; state = Out_of_date { old_value = None }
  ; has_cutoff =
      (match spec.allow_cutoff with
      | Yes _equal -> true
      | No -> false)
  }

let dep_node (t : (_, _) Table.t) input =
  match Store.find t.cache input with
  | Some dep_node -> dep_node
  | None ->
    let dep_node = make_dep_node ~spec:t.spec ~input in
    Store.set t.cache input dep_node;
    dep_node

let report_and_collect_errors f =
  Fiber.map_reduce_errors
    (module Collect_errors_monoid)
    ~on_error:(fun exn ->
      let exn, reproducible =
        match exn with
        | { Exn_with_backtrace.exn = Non_reproducible exn; backtrace } ->
          ({ Exn_with_backtrace.exn; backtrace }, false)
        | exn -> (exn, true)
      in
      let+ () = Error_handler.report_error exn in
      ({ exns = Exn_set.singleton exn; reproducible } : Collect_errors_monoid.t))
    f

let check_point = ref (Fiber.return ())

module Exec : sig
  val exec_dep_node : ('i, 'o) Dep_node.t -> 'o Fiber.t
end = struct
  let rec restore_from_cache :
            'o.
               cached_value:'o Cached_value.t
            -> 'o Cached_value.t Cache_lookup.Result.t Fiber.t =
   fun ~cached_value ->
    match cached_value.value with
    | Cancelled _dependency_cycle ->
      (* Dependencies of cancelled computations are not accurate (in fact, they
         are set to [Deps.empty]), so we can't use [deps_changed] in this
         case. *)
      Fiber.return
        (Cache_lookup.Result.Failure (Out_of_date { old_value = None }))
    | Error { reproducible = false; _ } ->
      (* We do not cache non-reproducible errors. *)
      Fiber.return
        (Cache_lookup.Result.Failure (Out_of_date { old_value = None }))
    | Ok _ | Error { reproducible = true; _ } -> (
      (* We cache reproducible errors just like normal values. We assume that
         all [Memo] computations are deterministic, which means if we rerun a
         computation that previously raised a set of errors on the same inputs
         then we expect to get the same set of errors back and might as well
         skip the unnecessary work. The downside is that if a computation is in
         fact non-deterministic, there is no way to force rerunning it, apart
         from changing some of its dependencies. *)
      let+ deps_changed =
        (* Make sure [f] gets inlined to avoid unnecessary closure allocations
           and improve stack traces in profiling. *)
        Deps.changed_or_not cached_value.deps
          ~f:(fun [@inline] (Dep_node.T dep) ->
            consider_and_restore_from_cache_without_adding_dep dep >>= function
            | Ok cached_value_of_dep -> (
              (* The [Changed] branch will be taken if [cached_value]'s node was
                 skipped in the previous run (it was unreachable), while [dep]
                 wasn't skipped and [cached_value_of_dep] changed. *)
              match
                Run.compare cached_value_of_dep.last_changed_at
                  cached_value.last_validated_at
              with
              | Gt -> Fiber.return Changed_or_not.Changed
              | Eq | Lt -> Fiber.return Changed_or_not.Unchanged)
            | Failure (Cancelled { dependency_cycle }) ->
              Fiber.return (Changed_or_not.Cancelled { dependency_cycle })
            | Failure (Out_of_date _old_value) -> (
              match dep.has_cutoff with
              | false ->
                (* If [dep] has no cutoff, it is sufficient to check whether it
                   is up to date. If not, we must recompute the
                   [cached_value]. *)
                Fiber.return Changed_or_not.Changed
              | true -> (
                (* If [dep] has a cutoff predicate, it is not sufficient to
                   check whether it is up to date: even if it isn't, after we
                   recompute it, the resulting value may remain unchanged,
                   allowing us to skip recomputing the [cached_value]. *)
                consider_and_compute_without_adding_dep dep
                >>| function
                | Ok cached_value_of_dep -> (
                  (* Note: [cached_value_of_dep.value] will be [Cancelled _] if
                     [dep] itself doesn't introduce a dependency cycle but one
                     of its transitive dependencies does. In this case, the
                     value will be new, so we will take the [Changed] branch. *)
                  match
                    Run.compare cached_value_of_dep.last_changed_at
                      cached_value.last_validated_at
                  with
                  | Gt -> Changed_or_not.Changed
                  | Eq | Lt -> Unchanged)
                | Error dependency_cycle -> Cancelled { dependency_cycle })))
      in
      match deps_changed with
      | Unchanged ->
        cached_value.last_validated_at <- Run.current ();
        Cache_lookup.Result.Ok cached_value
      | Changed -> Failure (Out_of_date { old_value = Some cached_value })
      | Cancelled { dependency_cycle } ->
        Failure (Cancelled { dependency_cycle }))

  and compute :
        'i 'o.
           dep_node:('i, 'o) Dep_node.t
        -> old_value:'o Cached_value.t option
        -> stack_frame:Stack_frame_with_state.t
        -> 'o Cached_value.t Fiber.t =
   fun ~dep_node ~old_value ~stack_frame ->
    let+ res =
      report_and_collect_errors (fun () ->
          let* () = !check_point in
          dep_node.without_state.spec.f dep_node.without_state.input)
    in
    let value =
      match res with
      | Ok res -> Value.Ok res
      | Error errors -> Error errors
    in
    let deps_rev = Stack_frame_with_state.deps_rev stack_frame in
    match old_value with
    | None -> Cached_value.create value ~deps_rev
    | Some old_cv -> (
      match Cached_value.value_changed dep_node old_cv.value value with
      | true -> Cached_value.create value ~deps_rev
      | false -> Cached_value.confirm_old_value ~deps_rev old_cv)

  and start_restoring :
        'i 'o.
           dep_node:('i, 'o) Dep_node.t
        -> cached_value:'o Cached_value.t
        -> 'o Cached_value.t Cache_lookup.Result.t Fiber.t =
   fun ~dep_node ~cached_value ->
    let computation = Computation.create () in
    dep_node.state <- Restoring { restore_from_cache = computation };
    Computation.force computation ~phase:Restore_from_cache
      ~dep_node:dep_node.without_state (fun _stack_frame ->
        let+ restore_result = restore_from_cache ~cached_value in
        if !Counters.enabled then incr Counters.nodes_restored;
        (match restore_result with
        | Ok cached_value -> dep_node.state <- Cached_value cached_value
        | Failure (Cancelled { dependency_cycle }) ->
          dep_node.state <-
            Cached_value (Cached_value.create_cancelled ~dependency_cycle)
        | Failure (Out_of_date { old_value }) ->
          dep_node.state <- Out_of_date { old_value });
        restore_result)

  and start_computing :
        'i 'o.
           dep_node:('i, 'o) Dep_node.t
        -> old_value:'o Cached_value.t option
        -> 'o Cached_value.t Fiber.t =
   fun ~dep_node ~old_value ->
    let computation = Computation.create () in
    dep_node.state <- Computing { old_value; compute = computation };
    Computation.force computation ~phase:Compute
      ~dep_node:dep_node.without_state (fun stack_frame ->
        let+ cached_value = compute ~dep_node ~old_value ~stack_frame in
        if !Counters.enabled then (
          incr Counters.nodes_computed;
          Counters.edges_traversed :=
            !Counters.edges_traversed + Deps.length cached_value.deps);
        dep_node.state <- Cached_value cached_value;
        cached_value)

  and consider_and_restore_from_cache_without_adding_dep :
        'i 'o.
        ('i, 'o) Dep_node.t -> 'o Cached_value.t Cache_lookup.Result.t Fiber.t =
   fun dep_node ->
    match dep_node.state with
    | Cached_value cached_value ->
      (* CR-someday amokhov: The happy path here is excruciatingly slow: read
         [dep_node.state], get [cached_value] via an indirection, jump through
         another hoop [cached_value.last_validated_at] and then, finally, make
         the [Run.is_current] check. We should try to find a shortcut. *)
      if Run.is_current cached_value.last_validated_at then
        Fiber.return (Cache_lookup.Result.Ok cached_value)
      else start_restoring ~dep_node ~cached_value
    | Restoring { restore_from_cache } -> (
      Computation.read_but_first_check_for_cycles restore_from_cache
        ~dep_node:dep_node.without_state
      >>| function
      | Ok res -> res
      | Error dependency_cycle ->
        Cache_lookup.Result.Failure (Cancelled { dependency_cycle }))
    | Out_of_date { old_value } | Computing { old_value; _ } ->
      Fiber.return (Cache_lookup.Result.Failure (Out_of_date { old_value }))

  (* This function assumes that restoring the value from cache failed. This
     means we should be in two possible states: [Out_of_date] or [Computing].
     However, as it turns out (see CR-someday below), [dep_node.state] can also
     contain an up-to-date [Cached_value]. We need to figure out why. *)
  and consider_and_compute_without_adding_dep :
        'i 'o.
        ('i, 'o) Dep_node.t -> ('o Cached_value.t, Cycle_error.t) result Fiber.t
      =
   fun dep_node ->
    match dep_node.state with
    | Cached_value cached_value ->
      (* CR-someday amokhov: We hit this branch in [dir-targets-promotion.t] but
         how is this possible? We need to investigate. For now, I'm replacing
         the [assert false] that we had here with [return (Ok cached_value)] if
         the [cached_value] turns out to be up to date. *)
      if not (Run.is_current cached_value.last_validated_at) then
        Code_error.raise "consider_and_compute_without_adding_dep"
          [ ("state", State.to_dyn dep_node.state)
          ; ("current run", Run.to_dyn (Run.current ()))
          ];
      Fiber.return (Ok cached_value)
    | Restoring _ -> assert false
    | Out_of_date { old_value } ->
      start_computing ~dep_node ~old_value >>| Result.ok
    | Computing { compute; _ } -> (
      Computation.read_but_first_check_for_cycles compute
        ~dep_node:dep_node.without_state
      >>| function
      | Ok _ as result -> result
      | Error dependency_cycle as result ->
        dep_node.state <-
          Cached_value (Cached_value.create_cancelled ~dependency_cycle);
        result)

  let exec_dep_node : 'i 'o. ('i, 'o) Dep_node.t -> 'o Fiber.t =
   fun dep_node ->
    Fiber.of_thunk (fun () ->
        let* result =
          consider_and_restore_from_cache_without_adding_dep dep_node
          >>= function
          | Ok cached_value -> Fiber.return (Ok cached_value)
          | Failure (Cancelled { dependency_cycle }) ->
            (* If restoring from cache failed with a cycle error, and the node's
               function is deterministic (as it should be), then we will hit the
               same cycle when trying to recompute the result. We therefore
               return the cycle error as is. Note that apart from saving some
               work, this also helps us work around the limitation of the cycle
               detection library that can't detect the same cycle twice. *)
            dep_node.state <-
              Cached_value (Cached_value.create_cancelled ~dependency_cycle);
            Fiber.return (Error dependency_cycle)
          | Failure (Out_of_date _) ->
            consider_and_compute_without_adding_dep dep_node
        in
        match result with
        | Ok res ->
          let* () = Call_stack.add_dep_from_caller dep_node in
          let stack_frame = Dep_node_without_state.T dep_node.without_state in
          Value.get_exn res.value ~stack_frame
        | Error cycle_error -> raise (Cycle_error.E cycle_error))
end

let exec (type i o) (t : (i, o) Table.t) i = Exec.exec_dep_node (dep_node t i)

let dump_cached_graph ?(on_not_cached = `Raise) ?(time_nodes = false) cell =
  let rec collect_graph (Dep_node.T dep_node) graph : Graph.t Fiber.t =
    let src_id = Id.to_int dep_node.without_state.id in
    match get_cached_value_in_current_run dep_node with
    | Some cached ->
      let* attributes =
        if time_nodes then
          let start = Unix.gettimeofday () in
          (* CR-someday cmoseley: We could record errors here and include them
             as part of the graph. *)
          let+ (_ : (_, Collect_errors_monoid.t) result) =
            report_and_collect_errors (fun () ->
                dep_node.without_state.spec.f dep_node.without_state.input)
          in
          let runtime = Unix.gettimeofday () -. start in
          String.Map.of_list_exn [ ("runtime", Graph.Attribute.Float runtime) ]
        else Fiber.return String.Map.empty
      in
      let graph =
        Graph.add_node graph ~id:src_id ?label:dep_node.without_state.spec.name
          ~attributes
      in
      List.fold_left (Deps.to_list cached.deps) ~init:(Fiber.return graph)
        ~f:(fun graph (Dep_node.T dst_node as packed) ->
          let* graph = graph in
          let dst_id = Id.to_int dst_node.without_state.id in
          let graph = Graph.add_edge graph ~src_id ~dst_id in
          if Graph.has_node graph ~id:dst_id then Fiber.return graph
          else collect_graph packed graph)
    | None -> (
      match on_not_cached with
      | `Raise -> failwith "Memo graph contains uncached nodes"
      | `Ignore -> Fiber.return graph)
  in
  Error_handler.with_error_handler
    (fun (_ : Exn_with_backtrace.t) -> Fiber.return ())
    (fun () -> collect_graph (Dep_node.T cell) Graph.empty)

let get_call_stack = Call_stack.get_call_stack_without_state

module Invalidation = struct
  (* This is currently used only for informing the user about the reason for
     restarting a build. *)
  module Reason = struct
    (* CR-someday amokhov: Add other reasons, e.g. a watched environment
       variable changed. *)
    type t =
      | Unknown
      | Path_changed of Path.t
      | Event_queue_overflow
      | Upgrade
      | Test

    let to_string_hum = function
      | Unknown -> None
      | Path_changed path -> Some (Path.to_string path ^ " changed")
      | Event_queue_overflow ->
        Some "Event queue overflow; full rebuild required"
      | Upgrade -> Some "Dune upgrader initiated a full rebuild"
      | Test -> Some "Rebuild initiated by an internal testsuite"
  end

  module Leaf = struct
    type kind =
      | Invalidate_node : _ Dep_node.t -> kind
      | Clear_cache : ('input, ('input, 'output) Dep_node.t) Store.t -> kind
      | Clear_caches

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
      match (a, b) with
      | Empty, x | x, Empty -> x
      | x, y -> Combine (x, y)
  end

  include T

  include (Monoid.Make (T) : Monoid.S with type t := t)

  let execute_leaf { Leaf.kind; _ } =
    match kind with
    | Invalidate_node dep_node -> invalidate_dep_node dep_node
    | Clear_cache store -> invalidate_store store
    | Clear_caches -> Caches.clear ()

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

  let rec to_list_x_xs x xs acc =
    match x with
    | Empty -> to_list_xs xs acc
    | Leaf f -> to_list_xs xs (f :: acc)
    | Combine (x, y) -> to_list_x_xs x (y :: xs) acc

  and to_list_xs xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> to_list_x_xs x xs acc

  let to_list t = to_list_x_xs t [] []

  let details_hum ?(max_elements = 1) t =
    assert (max_elements > 0);
    let details =
      List.filter_map ~f:Leaf.to_string_hum (to_list t)
      |> String.Set.of_list |> String.Set.to_list
    in
    (* CR-someday amokhov: Right now we just take first [max_elements] elements
       from the sorted list, but we could prioritise some reasons over others,
       e.g. if there is a global reset because of [Event_queue_overflow], it may
       be better to ensure that this reason is included. *)
    match List.truncate ~max_length:max_elements details with
    | `Not_truncated details when List.length details = 0 ->
      [ "Restarting for an unknown reason, please report it as a bug" ]
    | `Not_truncated details -> details
    | `Truncated truncated_details -> (
      let extra_message =
        let remaining_details = List.length details - max_elements in
        let plural =
          match remaining_details > 1 with
          | true -> "s"
          | false -> ""
        in
        sprintf ", and %d more change%s" remaining_details plural
      in
      match List.destruct_last truncated_details with
      | None -> assert false
      | Some (all_but_last, last) -> all_but_last @ [ last ^ extra_message ])

  let execute x = execute x []

  let is_empty = function
    | Empty -> true
    | _ -> false

  let clear_caches ~reason = Leaf { kind = Clear_caches; reason }

  let invalidate_cache ~reason ({ cache; _ } : _ Table.t) =
    Leaf { kind = Clear_cache cache; reason }

  let invalidate_node ~reason (dep_node : _ Dep_node.t) =
    Leaf { kind = Invalidate_node dep_node; reason }
end

module Current_run = struct
  let f () = Run.current () |> return

  let memo = create "current-run" ~input:(module Unit) f

  let exec () = exec memo ()

  let invalidate ~reason =
    Invalidation.invalidate_node ~reason (dep_node memo ())
end

let current_run () = Current_run.exec ()

let of_non_reproducible_fiber fiber =
  let* (_ : Run.t) = current_run () in
  fiber

let is_top_level =
  let+ is_set = Error_handler.is_set in
  not is_set

let run_with_error_handler t ~handle_error_no_raise =
  Error_handler.with_error_handler handle_error_no_raise (fun () ->
      let* res = report_and_collect_errors t in
      match res with
      | Ok ok -> Fiber.return ok
      | Error ({ exns; reproducible = _ } : Collect_errors_monoid.t) ->
        Fiber.reraise_all (Exn_set.to_list exns))

let run t =
  let* is_top_level = is_top_level in
  (* CR-someday aalekseyev: I think this automagical detection of toplevel calls
     is weird. My hunch is that having separate functions for toplevel and
     non-toplevel [run] would be better. *)
  match is_top_level with
  | true ->
    run_with_error_handler
      (fun () -> t)
      ~handle_error_no_raise:(fun _exn -> Fiber.return ())
  | false -> t

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

  let exec t = t
end

module Cell = struct
  type ('i, 'o) t = ('i, 'o) Dep_node.t

  let input (t : (_, _) t) = t.without_state.input

  let read = Exec.exec_dep_node

  let invalidate = Invalidation.invalidate_node
end

let cell = dep_node

module Implicit_output = Implicit_output

let lazy_cell ?cutoff ?name ?human_readable_description f =
  let spec =
    Spec.create ~name ~input:(module Unit) ~cutoff ~human_readable_description f
  in
  make_dep_node ~spec ~input:()

let push_stack_frame ~human_readable_description f =
  Cell.read (lazy_cell ~human_readable_description f)

module Lazy = struct
  type 'a t = unit -> 'a Fiber.t

  let of_val a () = Fiber.return a

  module Expert = struct
    let create ?cutoff ?name ?human_readable_description f =
      let cell = lazy_cell ?cutoff ?name ?human_readable_description f in
      (cell, fun () -> Cell.read cell)
  end

  let create ?cutoff ?name ?human_readable_description f =
    let (_ : (unit, 'a) Cell.t), t =
      Expert.create ?cutoff ?name ?human_readable_description f
    in
    t

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
      | T (id_v, res) -> (
        match Type_eq.Id.same id_v (id input_with_matching_id) with
        | None ->
          Code_error.raise
            "Type_eq.Id.t mismatch in Memo.Poly: the likely reason is that the \
             provided Function.id returns different ids for the same input."
            [ ("Function.name", Dyn.String name) ]
        | Some Type_eq.T -> res)
  end

  let memo =
    create name
      ~input:(module Key)
      (function
        | Key.T input -> eval input >>| fun v -> Value.T (id input, v))

  let eval x = exec memo (Key.T x) >>| Value.get ~input_with_matching_id:x
end

let reset invalidation =
  (* We rely on [invalidation] to list the actual reasons for the reset, which
     justifies the [~reason:Unknown] below. *)
  let invalidate_current_run = Current_run.invalidate ~reason:Unknown in
  Invalidation.execute
    (Invalidation.combine invalidation invalidate_current_run);
  Run.restart ();
  Counters.reset ()

module Perf_counters = struct
  let enable () = Counters.enabled := true

  let nodes_restored_in_current_run () = !Counters.nodes_restored

  let nodes_computed_in_current_run () = !Counters.nodes_computed

  let edges_traversed_in_current_run () = !Counters.edges_traversed

  let nodes_for_cycle_detection_in_current_run () =
    !Counters.nodes_in_cycle_detection_graph

  let edges_for_cycle_detection_in_current_run () =
    !Counters.edges_in_cycle_detection_graph

  let paths_for_cycle_detection_in_current_run () =
    !Counters.paths_in_cycle_detection_graph

  let report_for_current_run () =
    let memo =
      sprintf "Memo graph: %d/%d restored/computed nodes, %d traversed edges"
        (nodes_restored_in_current_run ())
        (nodes_computed_in_current_run ())
        (edges_traversed_in_current_run ())
    in
    let cycle_detection =
      sprintf "Memo cycle detection graph: %d/%d/%d nodes/edges/paths"
        (nodes_for_cycle_detection_in_current_run ())
        (edges_for_cycle_detection_in_current_run ())
        (paths_for_cycle_detection_in_current_run ())
    in
    String.concat ~sep:"\n" [ memo; cycle_detection ]

  let assert_invariants () =
    assert (
      nodes_for_cycle_detection_in_current_run ()
      <= nodes_computed_in_current_run () + nodes_restored_in_current_run ());
    assert (
      edges_for_cycle_detection_in_current_run ()
      <= edges_traversed_in_current_run ())

  let reset () = Counters.reset ()
end

module For_tests = struct
  let get_deps (type i o) (t : (i, o) Table.t) inp =
    match Store.find t.cache inp with
    | None -> None
    | Some dep_node -> (
      match get_cached_value_in_current_run dep_node with
      | None -> None
      | Some cv ->
        Some
          (Deps.to_list cv.deps
          |> List.map ~f:(fun (Dep_node.T dep) ->
                 ( dep.without_state.spec.name
                 , Dep_node_without_state.input_to_dyn dep.without_state ))))

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
