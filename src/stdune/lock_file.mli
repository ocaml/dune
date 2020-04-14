(** A mutually exclusive lock file. *)

type t

val create : Path.t -> t
(** [create path] creates a mutually exclusive lock file. The lock is held until
    the lock is released or this process ends. *)

val try_create : Path.t -> t option
(** Same as [create path] expect that [try_create path] returns [None] if the
    file is already locked by another process. *)

val unlock : t -> unit
(** [unlock t] releases lock file [t]. *)
