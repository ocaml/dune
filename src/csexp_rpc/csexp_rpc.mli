(** Canonical S-expression RPC.

    This module implements a RPC mechanism for exchanging canonical
    S-expressions over unix or internet sockets. It allows a server to accept
    connections and a client to connect to a server.

    However, it doesn't explain how to encode queries, responses or generally
    any kind of messages as Canonical S-expressions. This part should be built
    on top of this module.

    The actual system calls to connect to a server, accept connections, read and
    write messages are performed in separate threads. To integrate with the
    application scheduler, this module requires a way to post an ivar to fill
    ([Fiber.fill] value) from a separate system thread, and have the scheduler
    effectively fill this ivar from the main thread. *)

open Stdune

module Scheduler : sig
  (** Hook into the fiber scheduler. *)
  type t =
    { post_ivar_from_separate_thread : Fiber.fill -> unit
          (** Called from a separate thread to post an ivar to fill. *)
    ; register_pending_ivar : unit -> unit
          (** Called before any ivar is created *)
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

module Address : sig
  type ip =
    | V4
    | V6

  type port = int

  type t =
    | Unix of Path.t
    | Ip of ip * Unix.inet_addr * port
end

module Client : sig
  (** RPC Client *)
  type t

  val create : Address.t -> Scheduler.t -> t

  val stop : t -> unit

  val connect : t -> Session.t Fiber.t
end

module Server : sig
  (** RPC Server *)
  type t

  val create : Address.t -> backlog:int -> Scheduler.t -> t

  val stop : t -> unit

  val serve : t -> Session.t Fiber.Stream.In.t Fiber.t
end
