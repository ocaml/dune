(** The reduced set of file stats this module inspects to decide whether a file
    changed or not *)
type t =
  { mtime : Stdune.Time.t
  ; size : int
  ; perm : Unix.file_perm
  }

val to_dyn : t -> Dyn.t
val of_unix_stats : Unix.stats -> t
val of_time_stat : Stdune.Stat.t -> t
val compare : t -> t -> Ordering.t
