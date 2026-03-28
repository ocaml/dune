type t =
  { mtime : Time.t
  ; size : int
  ; perm : Unix.file_perm
  ; kind : Unix.file_kind
  ; dev : int
  ; ino : int
  }

val stat : string -> t
