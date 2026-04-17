(** Portable access [/dev/null] with some shared fd's to reduce the open fd
    count *)

val path : Path.t

(** [path] opened in read mode. Do not close this fd. *)
val in_ : Fd.t Lazy.t

(** [path] opened in write mode. Do not close this fd. *)
val out : Fd.t Lazy.t
