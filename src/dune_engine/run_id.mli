type t =
  | Batch
  | Watch of int

val equal : t -> t -> bool
val to_int : t -> int
