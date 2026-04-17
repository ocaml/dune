type t = Unix.file_descr

val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t
