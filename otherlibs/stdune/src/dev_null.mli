(** Portable access [/dev/null] with some shared fd's to reduce the open fd
    count *)

val path : Path.t

(** [path] opened in read mode. Do not close this fd. *)
val in_ : Unix.file_descr Lazy.t

(** [path] opened in write mode. Do not close this fd. *)
val out : Unix.file_descr Lazy.t
