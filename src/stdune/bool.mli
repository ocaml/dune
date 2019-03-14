type t = bool

val compare : t -> t -> Ordering.t

include Comparable.OPS with type t := t

val to_string : t -> string

val of_string : string -> t option
