module Instance : sig
  type ('r, 'u) t

  val update : ('r, 'u) t -> 'u -> unit Fiber.t

  val poll : ('r, 'u) t -> Dune_rpc_server.Poller.t -> 'r option Fiber.t

  val client_cancel : (_, _) t -> Dune_rpc_server.Poller.t -> unit Fiber.t
end

type t

val create : unit -> t

val disconnect_session : t -> _ Dune_rpc_server.Session.t -> unit Fiber.t

val progress :
  t -> (Dune_rpc_private.Progress.t, Dune_rpc_private.Progress.t) Instance.t

val diagnostic :
     t
  -> ( Dune_rpc_private.Diagnostic.Event.t list
     , Dune_engine.Build_config.Handler.error list )
     Instance.t
