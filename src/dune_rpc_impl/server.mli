type 'a t

val create
  :  lock_timeout:float option
  -> registry:[ `Add | `Skip ]
  -> root:string
  -> watch_mode_config:Watch_mode_config.t
  -> handle:(unit Dune_rpc_server.Handler.t -> unit)
       (** register additional requests or notifications *)
  -> Dune_stats.t option
  -> Dune_engine.Action_runner.Rpc_server.t
  -> parse_build:(string -> 'a)
  -> 'a t

type 'a pending_build_action = Build of 'a list * Decl.Build_outcome.t Fiber.Ivar.t

val pending_build_action : 'a t -> 'a pending_build_action Fiber.t

(** Stop accepting new rpc connections. Fiber returns when all existing
    connections terminate *)
val stop : _ t -> unit Fiber.t

val ready : _ t -> unit Fiber.t
val run : _ t -> unit Fiber.t
val action_runner : _ t -> Dune_engine.Action_runner.Rpc_server.t
