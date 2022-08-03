open! Stdune

val interpret_targets :
     Workspace_root.t
  -> Dune_config.t
  -> Dune_rules.Main.build_system
  -> Arg.Dep.t list
  -> unit Dune_engine.Action_builder.t

val expand_path_from_root :
     Workspace_root.t
  -> setup:Dune_rules.Main.build_system
  -> Dune_rules.Context.t
  -> Dune_lang.String_with_vars.t
  -> string Dune_engine.Action_builder.t
