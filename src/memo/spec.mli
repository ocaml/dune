(** A specification for a memoized function. *)

open! Import

module Allow_cutoff : sig
  type 'o t =
    | No
    | Yes of ('o -> 'o -> bool)
end

(** Events in the life-cycle of a memoized node, used for instrumentation. *)
module Event : sig
  type t =
    | Live (** The node became live, i.e. was used in the current run. *)
    | Validated
    (** The node's output was validated (confirmed up to date) in the current run. *)
end

type ('i, 'o) t =
  { name : string option
  ; (* If the field [witness] precedes any of the functional values ([input]
         and [f]), then polymorphic comparison actually works for [Spec.t]s. *)
    witness : 'i Type_eq.Id.t
  ; input : (module Store_intf.Input with type t = 'i)
  ; allow_cutoff : 'o Allow_cutoff.t
  ; f : 'i -> 'o Fiber.t
  ; human_readable_description : ('i -> User_message.Style.t Pp.t) option
  ; on_event : ('i -> Event.t -> unit) option
  }

val create
  :  name:string option
  -> input:(module Store_intf.Input with type t = 'a)
  -> human_readable_description:('a -> User_message.Style.t Pp.t) option
  -> cutoff:('b -> 'b -> bool) option
  -> ?on_event:('a -> Event.t -> unit)
  -> ('a -> 'b Fiber.t)
  -> ('a, 'b) t

(** [notify spec input event] runs [spec]'s [on_event] callback, if it has one. *)
val notify : ('i, _) t -> 'i -> Event.t -> unit
