type t = bool

val compare : t -> t -> Ordering.t

include Comparator.OPS with type t := t

val to_string : t -> string
val of_string : string -> t option
val to_dyn : t -> Dyn.t
val hash : t -> int
