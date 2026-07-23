type t =
  | Batch
  | Watch of int

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t
val to_int : t -> int
