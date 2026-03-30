open Import

type t =
  { mtime : Time.t
  ; size : int
  ; perm : Unix.file_perm
  }

let to_dyn { mtime; size; perm } =
  Dyn.Record [ "mtime", Int (Time.to_ns mtime); "size", Int size; "perm", Int perm ]
;;

let of_unix_stats (stats : Unix.stats) =
  { mtime = Time.of_epoch_secs stats.st_mtime
  ; size = stats.st_size
  ; perm = stats.st_perm
  }
;;

let of_time_stat (stats : Stat.t) =
  { mtime = stats.mtime; size = stats.size; perm = stats.perm }
;;

let compare { mtime; size; perm } t =
  let open Ordering.O in
  let= () = Time.compare mtime t.mtime in
  let= () = Int.compare size t.size in
  Int.compare perm t.perm
;;
