(** Element ordering *)

type t =
  | Lt (** Lesser than  *)
  | Eq (** Equal        *)
  | Gt (** Greater than *)

val of_int : int -> t
val to_int : t -> int

val neq : t -> bool
