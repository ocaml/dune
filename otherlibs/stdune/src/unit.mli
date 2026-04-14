type t = unit

val repr : t Repr.t
val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val hash : t -> int
val to_dyn : t -> Dyn.t
