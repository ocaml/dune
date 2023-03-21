open Import

type t

val create :
     lock_timeout:float option
  -> registry:[ `Add | `Skip ]
  -> root:string
  -> watch_mode_config:Watch_mode_config.t
  -> Dune_stats.t option
  -> Dune_engine.Action_runner.Rpc_server.t
  -> t

val listening_address : t -> Dune_rpc.Where.t

val stats : t -> Dune_stats.t option

type pending_build_action =
  | Build of Dune_rules.Dep_conf.t list * Decl.Build_outcome.t Fiber.Ivar.t

val pending_build_action : t -> pending_build_action Fiber.t

(** Stop accepting new rpc connections. Fiber returns when all existing
    connections terminate *)
val stop : t -> unit Fiber.t

val ready : t -> unit Fiber.t

val run : t -> unit Fiber.t

val action_runner : t -> Dune_engine.Action_runner.Rpc_server.t
