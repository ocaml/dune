open Import

type t

val create
  :  rpc_server:Dune_engine.Action_runner.Rpc_server.t
  -> where:Dune_rpc.Where.t
  -> config:Dune_config.t
  -> sandbox_actions:bool
  -> t

val runner : t -> Dune_engine.Action_runner.t
val start_worker : name:string -> where:string -> trace_fd:string option -> unit Fiber.t
