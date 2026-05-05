open Stdune

(** Readiness-based asynchronous IO on sockets (and pipes on Unix) implemented
    by running a small [Lev] loop in a dedicated thread.

    We still need a separate thread because the scheduler loop does not yet
    handle fd readiness and internal scheduler events in one place, so this
    module still has to synchronize access with a mutex. *)

type t := Types.Async_io.t

val create : Event.Queue.t -> t
val shutdown : t -> unit

(** [close fd] must be used to close any file descriptor which has been
    watched at some point. This is needed to make sure we never close a file
    descriptor that is being selected. Any associated operations with [fd] will
    be cancelled. The returned fiber is resolved only after [fd] has been
    closed on the async-io thread. *)
val close : Fd.t -> unit Fiber.t

module Task : sig
  (** A cancellable task *)
  type 'a t

  (** Cancel a running task *)
  val cancel : _ t -> unit Fiber.t

  (** Wait for a task to complete *)
  val await : 'a t -> ('a, [ `Cancelled | `Exn of exn ]) result Fiber.t
end

(** [sleep t after] schedules a one-shot timer on the async-io loop. *)
val sleep : t -> Time.Span.t -> unit Task.t

(** [ready fd what ~f] wait until [what] can be done on [fd] in a non-blocking
    way and then call [f]. Note that [f] will be called in a different thread,
    so it should only be used for atomic or synchronized operations. *)
val ready : Fd.t -> [ `Read | `Write ] -> f:(unit -> 'a) -> 'a Task.t

val ready_one
  :  ('label * Fd.t) list
  -> [ `Read | `Write ]
  -> f:('label -> Fd.t -> 'a)
  -> 'a Task.t

(** [connect fd sock] will do the equivalent of [Unix.connect fd sock] but
    without blocking. As in the other functions, you must call
    [Unix.set_nonblock fd] before calling this function.

    It's possible to implement this function using the other functions in this
    module. But since it's a bit non trivial, the implementation is done here. *)
val connect
  :  (Fd.t -> Unix.sockaddr -> unit)
  -> Fd.t
  -> Unix.sockaddr
  -> (unit, [ `Cancelled | `Exn of exn ]) result Fiber.t
