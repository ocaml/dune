open Stdune

type t =
  | File of Path.t
  | Alias of Alias.t

val request : t list -> unit Dune_engine.Action_builder.t

val resolve_targets_exn :
     Workspace_root.t
  -> Dune_config.t
  -> Dune_rules.Main.build_system
  -> Arg.Dep.t list
  -> t list Memo.Build.t
