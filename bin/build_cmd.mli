val run_build_command :
     common:Common.t
  -> config:Dune_config.t
  -> targets:(Dune_rules.Main.build_system -> Target.t list Memo.Build.t)
  -> unit

val runtest : unit Cmdliner.Term.t * Cmdliner.Term.info

val build : unit Cmdliner.Term.t * Cmdliner.Term.info
