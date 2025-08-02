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

module Option : sig
  type file_kind := t

  (* The values are constructed on the C-side *)
  type t =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK
    | UNKNOWN

  val elim : none:(unit -> 'a) -> some:(file_kind -> 'a) -> t -> 'a
end
