val run_build_command :
     common:Common.t
  -> targets:(Dune_rules.Main.build_system -> Target.t list)
  -> unit

val runtest : unit Cmdliner.Term.t * Cmdliner.Term.info

val build : unit Cmdliner.Term.t * Cmdliner.Term.info
