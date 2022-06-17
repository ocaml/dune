open Import

include
  Dune_rpc.Client.S
    with type 'a fiber := 'a Fiber.t
     and type chan := Csexp_rpc.Session.t

(** [client t where init ~on_notification ~f] Establishes a client connection to
    [where], initializes it with [init]. Once initialization is done, cals [f]
    with the active client. All notifications are fed to [on_notification]*)
val client :
     ?handler:Handler.t
  -> Dune_rpc.Where.t
  -> Dune_rpc.Initialize.Request.t
  -> f:(t -> 'a Fiber.t)
  -> 'a Fiber.t

(** Like [client], but start with an already-established session. *)
val client_with_session :
     ?handler:Handler.t
  -> Dune_rpc.Initialize.Request.t
  -> session:Csexp_rpc.Session.t
  -> f:(t -> 'a Fiber.t)
  -> 'a Fiber.t

module Connect : sig
  (** [csexp_client t path] connects to [path] and returns the client.

      This is needed for implementing low level functions such as
      [$ dune rpc init] *)
  val csexp_client : Dune_rpc.Where.t -> Csexp_rpc.Client.t Fiber.t
end
