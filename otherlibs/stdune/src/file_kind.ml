type t = Unix.file_kind =
  | S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK

let to_string = function
  | S_REG -> "S_REG"
  | S_DIR -> "S_DIR"
  | S_CHR -> "S_CHR"
  | S_BLK -> "S_BLK"
  | S_LNK -> "S_LNK"
  | S_FIFO -> "S_FIFO"
  | S_SOCK -> "S_SOCK"
;;

let to_string_hum = function
  | S_REG -> "regular file"
  | S_DIR -> "directory"
  | S_CHR -> "character device"
  | S_BLK -> "block device"
  | S_LNK -> "symbolic link"
  | S_FIFO -> "named pipe"
  | S_SOCK -> "socket"
;;

let equal x y =
  match x, y with
  | S_REG, S_REG -> true
  | S_REG, _ | _, S_REG -> false
  | S_DIR, S_DIR -> true
  | S_DIR, _ | _, S_DIR -> false
  | S_CHR, S_CHR -> true
  | S_CHR, _ | _, S_CHR -> false
  | S_BLK, S_BLK -> true
  | S_BLK, _ | _, S_BLK -> false
  | S_LNK, S_LNK -> true
  | S_LNK, _ | _, S_LNK -> false
  | S_FIFO, S_FIFO -> true
  | S_FIFO, _ | _, S_FIFO -> false
  | S_SOCK, S_SOCK -> true
;;

module Option = struct
  [@@@warning "-37"]

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

  let elim ~none ~some t =
    match t with
    | S_REG -> some (S_REG : Unix.file_kind)
    | S_DIR -> some S_DIR
    | S_CHR -> some S_CHR
    | S_BLK -> some S_BLK
    | S_LNK -> some S_LNK
    | S_FIFO -> some S_FIFO
    | S_SOCK -> some S_SOCK
    | UNKNOWN -> none ()
  ;;
end

let to_dyn t = Dyn.String (to_string t)
