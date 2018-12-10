type t

module Set : Set.S with type elt = t

val compare : t -> t -> Ordering.t

val to_hex : t -> string

val from_hex : string -> t

val file : Path.t -> t

val string : string -> t

val to_string_raw : t -> string
