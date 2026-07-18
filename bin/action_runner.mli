open Import

val create : config:Dune_config.t -> sandbox_actions:bool -> Dune_engine.Action_runner.t
val start_worker : name:string -> rpc_fd:string -> trace_fd:string option -> unit Fiber.t
