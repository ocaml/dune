val run_build_command :
     common:Common.t
  -> targets:(Dune_rules.Main.build_system -> Target.t list)
  -> unit

val run_build_command_once :
     common:Common.t
  -> targets:('a -> Target.t list)
  -> setup:(unit -> 'a Memo.Build.t)
  -> unit

val runtest : unit Cmdliner.Term.t * Cmdliner.Term.info

val build : unit Cmdliner.Term.t * Cmdliner.Term.info
