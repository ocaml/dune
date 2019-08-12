type t = string

module D = Dune_caml.Digest

module Set = String.Set

let hash = Hashtbl.hash

let equal = String.equal

let to_dyn s =
  let open Dyn.Encoder in
  constr "digest" [string s]

let file p = D.file (Path.to_string p)

let compare x y = Ordering.of_int (D.compare x y)

let to_string = D.to_hex

let from_hex s =
  match D.from_hex s with
  | s -> Some s
  | exception (Invalid_argument _) -> None

let string = D.string

let to_string_raw s = s

let generic a = string (Marshal.to_string a [])

let dir_digest (stat : Unix.stats) =
  generic (stat.st_size, stat.st_perm, stat.st_mtime, stat.st_ctime)

let path_stat_digest ?stat p =
  let stat = match stat with Some s -> s | None -> Path.stat p in
  let digest =
    if stat.Unix.st_kind = Unix.S_DIR then dir_digest stat
    else generic (file p, stat.st_perm land 0o100 (* Only take USR_X in account *))
  in
  (stat, digest)
