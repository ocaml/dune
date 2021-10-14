module type S = sig
  (** The type of internal state we keep per poller. The mental model should be
      that this represents the diff between the client's state and [current ()]. *)
  type state

  (** Type of response the long polling request expects *)
  type response

  (** The type of updates produced by the build system *)
  type update

  (** The current state of the build system. Used to initialize the client
      state. *)
  val current : unit -> response

  (** No update from the build system since last request *)
  val no_change : unit -> state

  (** Subsequent request by a long poller. May be delayed until the next update *)
  val on_rest_request :
    Dune_rpc_server.Poller.t -> state -> [ `Delay | `Respond of response ]

  (** An update for a poller without an active request *)
  val on_update_inactive : update -> state -> state option

  (** An update for all pollers that are waiting for a response *)
  val on_update_waiting : update -> response
end
