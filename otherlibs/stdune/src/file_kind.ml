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

let repr =
  Repr.variant
    "file-kind"
    [ Repr.case0 "S_REG" ~test:(function
        | S_REG -> true
        | _ -> false)
    ; Repr.case0 "S_DIR" ~test:(function
        | S_DIR -> true
        | _ -> false)
    ; Repr.case0 "S_CHR" ~test:(function
        | S_CHR -> true
        | _ -> false)
    ; Repr.case0 "S_BLK" ~test:(function
        | S_BLK -> true
        | _ -> false)
    ; Repr.case0 "S_LNK" ~test:(function
        | S_LNK -> true
        | _ -> false)
    ; Repr.case0 "S_FIFO" ~test:(function
        | S_FIFO -> true
        | _ -> false)
    ; Repr.case0 "S_SOCK" ~test:(function
        | S_SOCK -> true
        | _ -> false)
    ]
;;

let equal, _ = Repr.make_compare repr
let to_dyn = Repr.to_dyn repr
