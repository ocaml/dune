open Import

val build_via_rpc_server
  :  print_on_success:bool
  -> Dune_lang.Dep_conf.t list
  -> unit Fiber.t

val run_build_system
  :  common:Common.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> (unit, [ `Already_reported ]) result Fiber.t

val build : unit Cmd.t

val run_build_command
  :  common:Common.t
  -> config:Dune_config.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> unit
