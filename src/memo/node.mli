(** A node in the memoization graph: a memoized function applied to an input, together
    with its cached value, dependencies and lifecycle [State].

    Each node stores its [value], the [runs] in which the value last changed and was last
    validated, and the [deps] captured during its last computation. These are kept flat on
    the node (see {!Dep_node.t}) rather than inside the [State], so that a node keeps its
    value across state transitions (e.g. an invalidated node keeps its old value for the
    early cutoff check).

    A node's [State] evolves as follows:

    - A freshly created node starts in [Not_cached]. Computing it moves it to [Computing]
      and then to [Cached].

    - A [Cached] node may need to be revalidated in a later run. It first enters
      [Restoring] while we check whether any of its dependencies changed. If none did, it
      returns to [Cached] (early cutoff); otherwise it becomes [Out_of_date] and is then
      recomputed (via [Computing]) back to [Cached].

    - {!invalidate} marks a [Cached] node as [Out_of_date] directly.

    Only [Not_cached] fires a [Live] event on recompute; an [Out_of_date] node already
    fired [Live] when it started restoring. *)

(** Re-exported so that consumers that [open Node] can refer to the memoization metrics
    counters. Bound before [open! Import] so it resolves to Memo's own [Metrics] rather
    than [Stdune.Metrics]. *)
module Metrics = Metrics

open! Import
module Id : Id.S

module M : sig
  module Import : sig
    module Dag = Dag
  end

  module rec State : sig
    type t =
      | Cached
      | Not_cached
      | Out_of_date
      | Restoring of
          { restore_from_cache : Cycle_error.t Changed_or_not.t Computation0.t }
      | Computing of { compute : unit Computation0.t }
  end

  and Dep_node : sig
    type ('i, 'o) t =
      { id : Id.t
      ; spec : ('i, 'o) Spec.t
      ; input : 'i
      ; mutable state : State.t
      ; mutable value : 'o Value.t
      ; mutable runs : Run.Pair.t
      ; mutable deps : packed Deps.t
      }

    and packed = T : ('a, 'b) t -> packed [@@unboxed]
  end

  and Cycle_error : sig
    type t = Dep_node.packed list

    exception E of t
  end

  and Error : sig
    type t =
      { exn : exn
      ; rev_stack : Dep_node.packed list
      }

    exception E of t
  end

  and Dag : (Import.Dag.S with type value := Dep_node.packed)

  and Lazy_dag_node : sig
    type t = Dag.node Option.Unboxed.t ref
  end

  and Computation0 : sig
    type 'a t =
      { ivar : 'a Fiber.Ivar.t
      ; dag_node : Lazy_dag_node.t
      }
  end
end

module Dep_node : sig
  type ('i, 'o) t = ('i, 'o) M.Dep_node.t =
    { id : Id.t
      (** Placing [id] first makes polymorphic comparison for dep nodes correct regardless
        of the other fields. *)
    ; spec : ('i, 'o) Spec.t
    ; input : 'i
    ; mutable state : M.State.t
    ; mutable value : 'o Value.t
    ; mutable runs : Run.Pair.t
      (** [last_changed_at] and [last_validated_at] for early cutoff, packed into one
        immediate [Run.Pair.t]. [last_changed_at] is the run when the value last changed;
        [last_validated_at] is the run when it was last confirmed up to date. Invariant:
        [last_changed_at <= last_validated_at]. *)
    ; mutable deps : packed Deps.t
      (** The dependencies captured at [last_validated_at], in the order they were depended
        on. *)
    }

  and packed = M.Dep_node.packed = T : ('a, 'b) t -> packed [@@unboxed]

  val input_to_dyn : ('i, 'a) t -> Dyn.t
  val to_dyn_without_state : ('a, 'b) t -> Dyn.t
  val last_changed_at : ('a, 'b) t -> Run.t
  val last_validated_at : ('a, 'b) t -> Run.t

  module Packed : sig
    type t = packed

    val id : t -> Id.t
    val equal : t -> t -> bool
    val to_dyn_without_state : t -> Dyn.t
    val as_instance_of : t -> 'i Type_eq.Id.t -> 'i option
    val human_readable_description : t -> User_message.Style.t Pp.t option
  end
end

module Cycle_error : sig
  type t = Dep_node.packed list

  exception E of t

  val get : 'a -> 'a
  val to_dyn : Dep_node.packed list Dyn.builder
end

module Error : sig
  type t = M.Error.t =
    { exn : exn
    ; rev_stack : Dep_node.packed list
    }

  exception E of t

  val rotate_cycle : is_desired_head:('a -> bool) -> 'a list -> 'a list option

  val shorten_stack_leading_to_cycle
    :  rev_stack:Dep_node.packed list
    -> Dep_node.packed list
    -> Dep_node.packed list * Dep_node.packed list

  val get_exn_and_stack : t -> exn * Dep_node.packed list
  val get : t -> exn
  val stack : t -> Dep_node.packed list

  (** Push [stack_frame] onto the stack of an [Error.E] (wrapping a bare exception in
      [Error.E] if needed). *)
  val extend_stack : exn -> stack_frame:Dep_node.packed -> exn

  val to_dyn : t -> Dyn.t
end

module Exn_set = Value.Exn_set
module Collect_errors_monoid = Value.Collect_errors_monoid
module Dag = M.Dag

(** This is similar to [type t = Dag.node Lazy.t] but avoids creating a closure with a
    [dep_node]; the latter is available when we need to [force] a [t]. *)
module Lazy_dag_node : sig
  type t = Dag.node Option.Unboxed.t ref

  val create : unit -> 'a Option.Unboxed.t ref
  val force : Dag.node Option.Unboxed.t ref -> dep_node:Dep_node.packed -> Dag.node
end

(** A "computation" is represented by an [ivar], filled when the computation is finished,
    and a [dag_node], used for cycle detection before getting blocked on reading the
    [ivar]. *)
module Computation0 : sig
  type 'a t = 'a M.Computation0.t =
    { ivar : 'a Fiber.Ivar.t
    ; dag_node : M.Lazy_dag_node.t
    }

  val create : unit -> 'a t
end

val _print_dep_node : ?prefix:string -> ('a, 'b) Dep_node.t -> unit

(** Has the node's new value changed relative to its old value, according to the cutoff
    predicate? A fresh ([Uninitialized]) value always counts as changed, as does any
    transition into or out of a non-reproducible error. *)
val value_changed : ('a, 'b) Dep_node.t -> 'b Value.t -> 'b Value.t -> bool

(** Mark a node as up to date in the current run: its state becomes [Cached] and
    [last_validated_at] is bumped to the current run. *)
val validate_value : ('a, 'b) Dep_node.t -> unit

(** Store a freshly computed (or restored) [value] and [deps] on the node and mark it
    validated. If [value] is unchanged according to the cutoff, the old [last_changed_at]
    is kept (early cutoff). *)
val update_value
  :  ('a, 'b) Dep_node.t
  -> 'b Value.t
  -> deps:Dep_node.packed Deps.t
  -> unit

(** The dependencies of a node, if it is up to date in the current run. *)
val get_cached_deps_in_current_run : ('a, 'b) Dep_node.t -> Dep_node.packed Deps.t option

module State : sig
  type t = M.State.t =
    | Cached
    | Not_cached
    | Out_of_date
    | Restoring of
        { restore_from_cache : M.Cycle_error.t Changed_or_not.t Computation0.t }
    | Computing of { compute : unit Computation0.t }

  val to_dyn : t -> Dyn.t
end

(** Invalidate a node, marking it [Out_of_date] so it is reconsidered in the next run. A
    no-op on an already [Not_cached] or [Out_of_date] node; raises on a node that is
    [Restoring] or [Computing]. *)
val invalidate : ('a, 'b) Dep_node.t -> unit

module Stack_frame_with_state : sig
  type phase =
    | Restore_from_cache
    | Compute

  type t

  val to_dyn : t -> Dyn.t

  (** Create a new stack frame related to restoring or computing a [dep_node]. *)
  val create : dag_node:Lazy_dag_node.t -> phase -> dep_node:('a, 'b) Dep_node.t -> t

  val dep_node : t -> Dep_node.packed
  val dag_node : t -> Dag.node
  val children_added_to_dag : t -> Dag.Id.Set.t
  val record_child_added_to_dag : t -> dag_node_id:Dag.Id.t -> unit
end

module Call_stack : sig
  type t = Stack_frame_with_state.t list

  val call_stack_var : t option Fiber.Var.t
  val get_call_stack : unit -> t Fiber.t
  val get_call_stack_without_state : unit -> Dep_node.packed list Fiber.t
  val push_frame : Stack_frame_with_state.t -> (unit -> 'a Fiber.t) -> 'a Fiber.t
  val cycle_error_in_the_current_run : Cycle_error.t option ref
  val reset_cycle_error_in_the_current_run : unit -> unit

  (** Add all edges leading from the root of the call stack to [dag_node] to the cycle
      detection DAG. *)
  val add_path_to : dag_node:Dag.node -> (unit, Cycle_error.t) result Fiber.t
end

module Computation : sig
  type 'a t = 'a Computation0.t =
    { ivar : 'a Fiber.Ivar.t
    ; dag_node : M.Lazy_dag_node.t
    }

  val create : unit -> 'a t

  (** Force the computation exactly once, running [fiber] with a fresh stack frame for
      [dep_node]. Forcing it twice raises (the [ivar] is already filled); not forcing it
      leads to a deadlock. *)
  val force
    :  'a t
    -> phase:Stack_frame_with_state.phase
    -> dep_node:('b, 'c) Dep_node.t
    -> (Stack_frame_with_state.t -> 'a Fiber.t)
    -> 'a Fiber.t

  (** Read the result of a running computation, first checking for a dependency cycle
      (returning [Error] with the cycle if one is found). *)
  val read_but_first_check_for_cycles
    :  'a t
    -> phase:Stack_frame_with_state.phase
    -> dep_node:('b, 'c) Dep_node.t
    -> ('a, Cycle_error.t) result Fiber.t
end
