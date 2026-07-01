type t

val of_string : string -> t
val hash : t -> int
val to_dyn : t -> Dyn.t
val equal : t -> t -> bool
val parse_string_exn : Loc.t * string -> t
val to_string : t -> string
val repr : t Repr.t
