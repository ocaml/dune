module type S = sig
  (** type of internal state we keep per poller *)
  type state

  (** type of response the long polling request expects *)
  type response

  (** the type of updates we push down to pollers *)
  type update

  val current : unit -> response

  (** initialize a new state for a waiting client *)
  val init_state : unit -> state

  (** called after the state is used to produce a response *)
  val reset : unit -> state

  (** subsequent request by a long poller. may be delayed until the next update *)
  val on_rest_request :
    Dune_rpc_server.Poller.t -> state -> [ `Delay | `Respond of response ]

  (** an update for a poller without an active request *)
  val on_update_inactive : update -> state -> state option

  (** an update for all pollers that are waiting for a response *)
  val on_update_waiting : update -> response
end
