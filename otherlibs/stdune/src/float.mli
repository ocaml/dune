type t = float

val repr : t Repr.t
val of_string : string -> t option
val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val max : t -> t -> t
val to_dyn : t -> Dyn.t
