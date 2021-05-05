open! Stdune
open Import

(** Rpc related functions *)

module Config : sig
  type t =
    | Client
    | Server of
        { handler : Dune_rpc_server.t
        ; pool : Fiber.Pool.t
        ; backlog : int
        }
end

type t

val t : unit -> t Fiber.t

val of_config : Config.t -> Csexp_rpc.Scheduler.t -> Dune_stats.t option -> t

(** Stop accepting new rpc connections. Fiber returns when all existing
    connetions terminate *)
val stop : unit -> unit Fiber.t

val run : t -> unit Fiber.t

(** [client t where init ~on_notification ~f] Establishes a client connection to
    [where], initializes it with [init]. Once initialization is done, cals [f]
    with the active client. All notifications are fed to [on_notification]*)
val client :
     t
  -> Dune_rpc.Where.t
  -> Dune_rpc.Initialize.Request.t
  -> on_notification:(Dune_rpc.Call.t -> unit Fiber.t)
  -> f:(Client.t -> 'a Fiber.t)
  -> 'a Fiber.t

module Connect : sig
  (** [csexp_client t path] connects to [path] and returns the client.

      This is needed for implementing low level functions such as
      [$ dune rpc init] *)
  val csexp_client : t -> Dune_rpc.Where.t -> Csexp_rpc.Client.t

  (** [csexp_connect i o] creates a session where requests are read from [i] and
      responses are written to [o].

      This is needed for implementing low level functions such as
      [$ dune rpc init] *)
  val csexp_connect : t -> in_channel -> out_channel -> Csexp_rpc.Session.t

  val connect_persistent :
       t
    -> (Csexp_rpc.Session.t Fiber.Stream.In.t * Csexp_rpc.Client.t option)
       Fiber.t
end
