type t =
  { mtime : Time.t
  ; size : int
  ; perm : Unix.file_perm
  ; kind : Unix.file_kind
  ; dev : int
  ; ino : int
  }

let compare = Poly.compare

external stat : string -> t = "dune_stat"
