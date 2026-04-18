open Import

val create
  :  Dune_rpc_impl.Server.t
  -> config:Dune_config.t
  -> sandbox_actions:bool
  -> Dune_engine.Action_runner.t

val start_worker : name:string -> where:string -> trace_fd:string option -> unit Fiber.t
