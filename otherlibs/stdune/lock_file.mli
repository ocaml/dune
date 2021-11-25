(** A mutually exclusive lock file. *)

type t

(** [create path] creates a mutually exclusive lock file. The lock is held until
    the lock is released or this process ends. *)
val create : Path.t -> t

(** Same as [create path] expect that [try_create path] returns [None] if the
    file is already locked by another process. *)
val try_create : Path.t -> t option

(** [unlock t] releases lock file [t]. *)
val unlock : t -> unit
