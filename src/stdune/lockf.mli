(** A mutually exclusive lock on a file. *)
type t

val lock : string -> t
(** [lock path] creates a mutually exclusive lock on the file referred by
[path], creating it if it does not exist, and returns it. Until the lock is
released or this process ends, [lock] will return [None] for process trying to
lock the file. *)

val lock_try : string -> t option
(** [lock path] creates a mutually exclusive lock on the file referred by
[path], creating it if it does not exist, and returns it. Until the lock is
released or this process ends, [lock] will return [None] for process trying to
lock the file. *)

val unlock : t -> unit
(** [unlock l] releases lock [l]. *)
