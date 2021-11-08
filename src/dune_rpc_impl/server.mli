type t

val create : root:string -> t

val config : t -> Run.Config.t

val build_handler : t -> Dune_engine.Build_system.Handler.t

val acknowledge_build_starting : t -> unit Fiber.t

val acknowledge_build_finished : t -> Decl.Build_outcome.t -> unit Fiber.t
