open Dune_engine

val run_build_command :
     common:Common.t
  -> config:Dune_config.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> unit

val runtest : unit Cmdliner.Term.t * Cmdliner.Term.info

val build : unit Cmdliner.Term.t * Cmdliner.Term.info

val fmt : unit Cmdliner.Term.t * Cmdliner.Term.info
