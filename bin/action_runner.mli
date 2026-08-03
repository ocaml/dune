open Import

val create : config:Dune_config.t -> sandbox_actions:bool -> Dune_engine.Action_runner.t

val start_worker
  :  name:Action_runner_name.t
  -> rpc_fd:Fd.t
  -> trace_fd:Fd.t option
  -> unit Fiber.t
