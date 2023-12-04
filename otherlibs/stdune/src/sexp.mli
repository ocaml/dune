(** S-expressions *)

type t = Csexp.t =
  | Atom of string
  | List of t list

val to_string : t -> string
val pp : t -> 'a Pp.t
val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val of_dyn : Dyn.t -> t
val to_dyn : t -> Dyn.t
val record : (string * t) list -> t
