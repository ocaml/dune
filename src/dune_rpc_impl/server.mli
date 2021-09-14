type t

val create : unit -> t

val config : t -> Run.Config.t

val build_handler : t -> Dune_engine.Build_system.Handler.t

type pending_build_action =
  | Build of Dune_rules.Dep_conf.t list * Decl.Build_outcome.t Fiber.Ivar.t

val pending_build_action : t -> pending_build_action Fiber.t
