open Import

val run_build_system
  :  common:Common.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> (unit, [ `Already_reported ]) result Fiber.t

val runtest : unit Cmd.t
val runtest_term : unit Term.t
val build : unit Cmd.t
val fmt : unit Cmd.t

val run_build_command
  :  common:Common.t
  -> config:Dune_config.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> unit
