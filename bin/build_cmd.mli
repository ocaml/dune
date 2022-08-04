open Import

val run_build_command :
     common:Common.t
  -> config:Dune_config.t
  -> request:(Main.build_system -> unit Action_builder.t)
  -> unit

val runtest : unit Cmd.t

val runtest_term : unit Term.t

val build : unit Cmd.t

val fmt : unit Cmd.t
