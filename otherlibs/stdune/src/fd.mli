type t

val repr : t Repr.t
val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t
val close : t -> unit
val set_close_on_exec : t -> unit
val clear_close_on_exec : t -> unit

(** Unsafe casts to bridge callers that still use [Unix.file_descr]. *)
val unsafe_of_int : int -> Unix.file_descr

val unsafe_to_int : Unix.file_descr -> int
val unsafe_of_unix_file_descr : Unix.file_descr -> t
val unsafe_to_unix_file_descr : t -> Unix.file_descr
