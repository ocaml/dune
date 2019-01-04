type t = bool

val compare : t -> t -> Ordering.t

val to_string : t -> string

val of_string : string -> t option
