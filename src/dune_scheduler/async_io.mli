(** Poor man's asynchronous IO on sockets (and pipes on Unix)

    Problematic in three ways:

    - Needs to run in a separate thread because our scheduler loop does not
      allow polling for fd's and custom events. This requires unnecessary
      locking.

    - Uses the rather slow select primitive. There's much better options on
      every operating system.

    - Relies on the "pipe trick" to be interruptible. This is the best we can do
      with select. *)

(* TODO one day switch to lev and integrate all of this directly into the
   scheduler. This should solve all the problems above. *)

module type Scheduler = sig
  val fill_jobs : Fiber.fill list -> unit
  val register_job_started : unit -> unit
  val cancel_job_started : unit -> unit
  val spawn_thread : (unit -> unit) -> unit
end

(** [with_io scheduler f] runs [f] with [scheduler]. All operations in this
    module must be executed inside [f]. *)
val with_io : (module Scheduler) -> (unit -> 'a Fiber.t) -> 'a Fiber.t

(** [close fd] must be used to close any file descriptor which has been watched
    at some point. This is needed to make sure we never close a file descriptor
    that is being selected. Any associated operations with [fd] will be
    cancelled. *)
val close : Unix.file_descr -> unit Fiber.t

module Task : sig
  (** A cancellable task *)
  type 'a t

  (** Cancel a running task *)
  val cancel : _ t -> unit Fiber.t

  (** Wait for a task to complete *)
  val await : 'a t -> ('a, [ `Cancelled | `Exn of exn ]) result Fiber.t
end

(** [ready fd what ~f] wait until [what] can be done on [fd] in a non-blocking
    way and then call [f]. Note that [f] will be called in a different thread,
    so it should only be used for atomic or synchronized operations. *)
val ready : Unix.file_descr -> [ `Read | `Write ] -> f:(unit -> 'a) -> 'a Task.t Fiber.t

val ready_one
  :  ('label * Unix.file_descr) list
  -> [ `Read | `Write ]
  -> f:('label -> Unix.file_descr -> 'a)
  -> 'a Task.t Fiber.t

(** [connect fd sock] will do the equivalent of [Unix.connect fd sock] but
    without blocking. As in the other functions, you must call
    [Unix.set_nonblock fd] before calling this function.

    It's possible to implement this function using the other functions in this
    module. But since it's a bit non trivial, the implementation is done here. *)
val connect
  :  (Unix.file_descr -> Unix.sockaddr -> unit)
  -> Unix.file_descr
  -> Unix.sockaddr
  -> (unit, [ `Cancelled | `Exn of exn ]) result Fiber.t
