(** Canonical S-expression RPC.

    This module implements a RPC mechanism for exchanging canonical
    S-expressions over unix or internet sockets. It allows a server to accept
    connections and a client to connect to a server.

    However, it doesn't explain how to encode queries, responses or generally
    any kind of messages as Canonical S-expressions. This part should be built
    on top of this module.

    The actual system calls to connect to a server, accept connections, read and
    write messages are performed in separate threads. To integrate with the
    application scheduler, this module requires a way to create an ivar than can
    be filled from a separate system thread. *)

open Stdune

module Scheduler : sig
  (** Hook into the fiber scheduler. *)
  type t =
    { create_thread_safe_ivar : 'a. unit -> 'a Fiber.Ivar.t * ('a -> unit)
          (** Create a thread safe Ivar. The returned function must be called
              from a separate thread in order to fill the ivar. *)
    ; spawn_thread : (unit -> unit) -> unit
          (** We spawn threads through this function in case the scheduler wants
              to block signals *)
    }
end

module Session : sig
  (** Rpc session backed by two threads. One thread for reading, and another for
      writing *)
  type t

  val create : in_channel -> out_channel -> Scheduler.t -> t

  (* [write t x] writes the s-expression when [x] is [Some sexp], and closes the
     session if [x = None ] *)
  val write : t -> Sexp.t option -> unit Fiber.t

  (** If [read] returns [None], the session is closed and all subsequent reads
      will return [None] *)
  val read : t -> Sexp.t option Fiber.t
end

module Client : sig
  (** RPC Client *)
  type t

  val create : Unix.sockaddr -> Scheduler.t -> t

  val stop : t -> unit

  val connect : t -> Session.t Fiber.t
end

module Server : sig
  (** RPC Server *)
  type t

  val create : Unix.sockaddr -> backlog:int -> Scheduler.t -> t

  val stop : t -> unit

  val serve : t -> Session.t Fiber.Stream.In.t Fiber.t

  val listening_address : t -> Unix.sockaddr
end
