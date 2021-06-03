open Stdune

type t =
  | File of Path.t
  | Alias of Alias.t

val request : t list -> unit Dune_engine.Action_builder.t

type resolve_input =
  | Path of Path.t
  | Dep of Arg.Dep.t

val resolve_targets_mixed :
     Workspace_root.t
  -> Dune_config.t
  -> Dune_rules.Main.build_system
  -> resolve_input list
  -> (t list, Arg.Dep.t * User_message.Style.t Pp.t list) result list
     Memo.Build.t

val resolve_targets_exn :
     Workspace_root.t
  -> Dune_config.t
  -> Dune_rules.Main.build_system
  -> Arg.Dep.t list
  -> t list Memo.Build.t
