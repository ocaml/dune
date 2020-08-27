open Stdune

type t =
  | File of Path.t
  | Alias of Alias.t

val request : t list -> unit Dune_engine.Build.t

val resolve_target :
     Common.t
  -> setup:Dune_rules.Main.build_system
  -> Arg.Dep.t
  -> (t list, Arg.Dep.t * User_message.Style.t Pp.t list) result

type resolve_input =
  | Path of Path.t
  | Dep of Arg.Dep.t

val resolve_targets_mixed :
     Common.t
  -> Dune_rules.Main.build_system
  -> resolve_input list
  -> (t list, Arg.Dep.t * User_message.Style.t Pp.t list) result list

val resolve_targets_exn :
  Common.t -> Dune_rules.Main.build_system -> Arg.Dep.t list -> t list
