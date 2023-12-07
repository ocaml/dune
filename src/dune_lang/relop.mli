type t =
  | Eq
  | Gte
  | Lte
  | Gt
  | Lt
  | Neq

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val to_string : t -> string
val map : (string * t) list
val encode : t -> Dune_sexp.t
val eval : t -> Ordering.t -> bool
