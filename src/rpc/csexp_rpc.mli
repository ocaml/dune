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

module Session : sig
  (** Rpc session backed by two threads. One thread for reading, and another for
      writing *)
  type t

  val close : t -> unit Fiber.t

  (** A private Unix socket session with file descriptor passing enabled. *)
  val of_fd : Fd.t -> t

  module Fds : sig
    type t
  end

  (** Send descriptors before the request that references them. The caller
      retains ownership of its copies. *)
  val send_fds : t -> Fd.t list -> (Fds.t, [ `Closed ]) result Fiber.t

  (** Claim the received copies. The caller must close them. *)
  val take_fds : t -> Fds.t -> Fd.t array

  (** [write t xs] writes the s-expressions [xs]. *)
  val write : t -> Sexp.t list -> (unit, [ `Closed ]) result Fiber.t

  (** If [read] returns [None], the session is closed and all subsequent reads
      will return [None] *)
  val read : t -> Sexp.t option Fiber.t
end

module Client : sig
  (** RPC Client *)
  type t

  val create : Unix.sockaddr -> t
  val stop : t -> unit
  val connect_exn : t -> Session.t Fiber.t
  val connect : t -> (Session.t, Exn_with_backtrace.t) result Fiber.t
end

module Server : sig
  (** RPC Server *)
  type t

  (** [create sockaddrs ~backlog] binds and starts listening on [sockaddrs]. *)
  val create : Unix.sockaddr list -> backlog:int -> (t, [ `Already_in_use ]) result

  (** [stop t] completes only after the listening sockets are closed and no new
      clients can connect. *)
  val stop : t -> unit Fiber.t

  (** [serve t] accepts clients from [t]. It must be called at most once. *)
  val serve : t -> Session.t Fiber.Stream.In.t Fiber.t

  val listening_address : t -> Unix.sockaddr list
end
