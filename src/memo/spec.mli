(** A specification for a memoized function. *)

open! Import

(** Events in the life-cycle of a memoized node, used for instrumentation. *)
module Event : sig
  type t =
    | Live (** The node became live, i.e. was used in the current run. *)
    | Validated
    (** The node's output was validated (confirmed up to date) in the current run. *)
end

(** Memo nodes can have some special features, for example, [cutoff]
    predicates. *)
module Node_kind : sig
  type ('i, 'o) t
end

type ('i, 'o) t =
  { name : string option
  ; (* [witness] is [Some] only for named tables, which may be downcast to their
         input type via [as_instance_of]; it is [None] for lazy values, stack
         frames, and variables, avoiding a [Type_eq.Id.t] allocation per such cell.
         If [witness] precedes the functional values ([input], [f], and the
         closures inside [node_kind]), polymorphic comparison works for [Spec.t]s. *)
    witness : 'i Type_eq.Id.t option
  ; input : (module Store_intf.Input with type t = 'i)
  ; node_kind : ('i, 'o) Node_kind.t
  ; f : 'i -> 'o Fiber.t
  ; human_readable_description : ('i -> User_message.Style.t Pp.t option) option
  }

val create
  :  name:string option
  -> input:(module Store_intf.Input with type t = 'a)
  -> human_readable_description:('a -> User_message.Style.t Pp.t option) option
  -> cutoff:('b -> 'b -> bool) option
  -> ?witness:bool
  -> ?on_event:('a -> Event.t -> unit)
  -> ('a -> 'b Fiber.t)
  -> ('a, 'b) t

(** Does the [new_value] differ from the [old_value]? Always [true] for nodes
    without a cutoff predicate. *)
val output_changed : (_, 'o) t -> old_value:'o -> new_value:'o -> bool

(** Whether the node has a cutoff predicate. If [false], [output_changed] is
    guaranteed to return [true] for any pair of values. *)
val has_cutoff : _ t -> bool

(** [notify spec input event] runs [spec]'s [on_event] callback, if it has one. *)
val notify : ('i, _) t -> 'i -> Event.t -> unit
