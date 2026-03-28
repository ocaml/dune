open Import

type t =
  { mtime : float
  ; size : int
  ; perm : Unix.file_perm
  }

let to_dyn { mtime; size; perm } =
  Dyn.Record [ "mtime", Float mtime; "size", Int size; "perm", Int perm ]
;;

let of_unix_stats (stats : Unix.stats) =
  { mtime = stats.st_mtime; size = stats.st_size; perm = stats.st_perm }
;;

let compare { mtime; size; perm } t =
  let open Ordering.O in
  let= () = Float.compare mtime t.mtime in
  let= () = Int.compare size t.size in
  Int.compare perm t.perm
;;
