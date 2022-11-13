(** global lock shared between dune processes.

    Before starting rpc, writing to the build dir, this lock should be locked. *)

(** attempt to acquire a lock. once a lock is locked, subsequent locks always
    succeed *)
val lock_exn : timeout:float option -> unit

(** release a lock and allow it be re-acquired *)
val unlock : unit -> unit
