open Import

val run_build_system
  :  action_runner:Dune_engine.Action_runner.t option
  -> run_id:Dune_engine.Run_id.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> (unit, [ `Already_reported ]) result Fiber.t

val build : unit Cmd.t

val run_build_command
  :  common:Common.t
  -> config:Dune_config.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> unit

val build_memo_exn : (unit -> 'a Memo.t) -> 'a Fiber.t

val describe
  :  Common.Builder.t
  -> context_name:Dune_engine.Context_name.t
  -> (Common.t -> Dune_rules.Main.build_system -> Super_context.t -> 'a Memo.t)
  -> 'a
