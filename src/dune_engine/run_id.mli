type t =
  | Batch
  | Watch of int

val to_int : t -> int
