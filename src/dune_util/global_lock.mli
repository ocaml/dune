(** global lock shared between dune processes.

    Before starting rpc, writing to the build dir, this lock should be locked. *)

(** attempt to acquire a lock. once a lock is locked, subsequent locks always
    succeed. Returns [Ok ()] if the lock is acquired within [timeout] seconds,
    and [Error ()] otherwise. *)
val lock : timeout:float option -> (unit, unit) result

val lock_exn : timeout:float option -> unit

(** release a lock and allow it be re-acquired *)
val unlock : unit -> unit

val write_pid : Unix.file_descr -> unit
