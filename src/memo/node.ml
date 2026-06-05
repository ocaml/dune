module Metrics0 = Metrics
open! Import
module Metrics = Metrics0
module Graph = Dune_graph.Graph
module Console = Console
module Debug = Memo_debug
module Event = Spec.Event
include Fiber
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
      (match node.spec.allow_cutoff with
       | Yes equal -> not (equal prev_value cur_value)
       | No -> true)
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
