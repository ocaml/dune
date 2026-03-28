(** The reduced set of file stats this module inspects to decide whether a file
    changed or not *)
type t =
  { mtime : float
  ; size : int
  ; perm : Unix.file_perm
  }

val to_dyn : t -> Dyn.t
val of_unix_stats : Unix.stats -> t
val compare : t -> t -> Ordering.t
