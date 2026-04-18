type t

val of_string : string -> t
val parse_string_exn : Loc.t * string -> t
val to_string : t -> string
val repr : t Repr.t
