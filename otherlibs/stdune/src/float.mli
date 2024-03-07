type t = float

val of_string : string -> t option
val to_string : t -> string
val compare : t -> t -> Ordering.t
val max : t -> t -> t
