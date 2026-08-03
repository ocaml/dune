(** global lock shared between dune processes.

    Before starting rpc, writing to the build dir, this lock should be locked. *)

module Lock_held_by : sig
  type t =
    | Pid_from_lockfile of int
    | Unknown
end

(** Attempt to acquire a lock. Once a lock is locked, subsequent locks always
    succeed. Returns [Ok ()] if the lock is acquired and [Error] when another
    dune process has acquired the lock. *)
val lock : unit -> (unit, Lock_held_by.t) result

val lock_exn : unit -> unit

(** release a lock and allow it be re-acquired *)
val unlock : unit -> unit

val write_pid : Fd.t -> unit
val at_exit : At_exit.t
