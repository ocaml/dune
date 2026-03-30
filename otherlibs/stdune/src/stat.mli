type t =
  { mtime : Time.t
  ; size : int
  ; perm : Unix.file_perm
  ; kind : Unix.file_kind
  ; dev : int
  ; ino : int
  }

val compare : t -> t -> Ordering.t
val stat : string -> t
