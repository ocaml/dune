type t =
  | Simple of bool
  | Yes_with_transition of string

val equal : t -> t -> bool

include Dune_sexp.Conv.S with type t := t

val to_bool : t -> bool
val to_dyn : t -> Dyn.t
