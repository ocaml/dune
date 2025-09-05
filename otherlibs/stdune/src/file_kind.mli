type t = Unix.file_kind =
  | S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK

val to_string : t -> string
val to_string_hum : t -> string
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
