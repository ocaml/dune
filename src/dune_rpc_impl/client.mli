open Import
module Client := Dune_rpc_client.Client

(** [client t where init ~on_notification ~f] Establishes a client connection to
    [where], initializes it with [init]. Once initialization is done, cals [f]
    with the active client. All notifications are fed to [on_notification]*)
val client
  :  ?handler:Client.Handler.t
  -> Client.Connection.t
  -> Dune_rpc.Initialize.Request.t
  -> f:(Client.t -> 'a Fiber.t)
  -> 'a Fiber.t
