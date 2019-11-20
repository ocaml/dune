type t = string

module D = Stdlib.Digest
module Set = String.Set

let hash = Hashtbl.hash

let equal = String.equal

let to_dyn s =
  let open Dyn.Encoder in
  constr "digest" [ string s ]

let file p = D.file (Path.to_string p)

let compare x y = Ordering.of_int (D.compare x y)

let to_string = D.to_hex

let from_hex s =
  match D.from_hex s with
  | s -> Some s
  | exception Invalid_argument _ -> None

let string = D.string

let to_string_raw s = s

let generic a = string (Marshal.to_string a [])

let file_with_stats p (stats : Unix.stats) =
  match stats.st_kind with
  | S_DIR ->
    generic (stats.st_size, stats.st_perm, stats.st_mtime, stats.st_ctime)
  | _ ->
    generic (file p, stats.st_perm land 0o100 (* Only take USR_X in account *))
