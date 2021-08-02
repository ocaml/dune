open! Stdune

val interpret_targets :
     Workspace_root.t
  -> Dune_config.t
  -> Dune_rules.Main.build_system
  -> Arg.Dep.t list
  -> unit Dune_engine.Action_builder.t
