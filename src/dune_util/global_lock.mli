(** global lock shared between dune processes.

    Before starting rpc, writing to the build dir, this lock should be locked. *)

module Lock_held_by : sig
  type t

  (** returns " (pid: X)" where X is the PID if the PID is known, otherwise the
      empty string *)
  val to_string_empty_if_unknown : t -> string
end

(** Attempt to acquire a lock. once a lock is locked, subsequent locks always
    succeed. Returns [Ok ()] if the lock is acquired within [timeout] seconds,
    and [Error ()] otherwise. *)
val lock : timeout:float option -> (unit, Lock_held_by.t) result

val lock_exn : timeout:float option -> unit

(** release a lock and allow it be re-acquired *)
val unlock : unit -> unit

val write_pid : Unix.file_descr -> unit
