open Import

include Dune_rpc.Client.S with type 'a fiber := 'a Fiber.t

module Connection : sig
  type t

  val connect : Dune_rpc.Where.t -> (t, User_message.t) result Fiber.t

  (** like [connect] but fails with a nice error message for the user *)
  val connect_exn : Dune_rpc.Where.t -> t Fiber.t
end

(** [client t where init ~on_notification ~f] Establishes a client connection to
    [where], initializes it with [init]. Once initialization is done, cals [f]
    with the active client. All notifications are fed to [on_notification]*)
val client :
     ?handler:Handler.t
  -> Connection.t
  -> Dune_rpc.Initialize.Request.t
  -> f:(t -> 'a Fiber.t)
  -> 'a Fiber.t
