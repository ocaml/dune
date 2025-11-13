(** A specification for a memoized function. *)

open! Import

module Allow_cutoff : sig
  type 'o t =
    | No
    | Yes of ('o -> 'o -> bool)
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
  }

val create
  :  name:string option
  -> input:(module Store_intf.Input with type t = 'a)
  -> human_readable_description:('a -> User_message.Style.t Pp.t) option
  -> cutoff:('b -> 'b -> bool) option
  -> ('a -> 'b Fiber.t)
  -> ('a, 'b) t
