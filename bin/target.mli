open Stdune

type t =
  | File of Path.t
  | Alias of Alias.t

val request : Dune.Main.build_system -> t list -> (unit, unit) Dune.Build.t

val resolve_target :
     Common.t
  -> setup:Dune.Main.build_system
  -> Arg.Dep.t
  -> (t list, Arg.Dep.t * User_message.Style.t Pp.t list) result

type resolve_input =
  | Path of Path.t
  | Dep of Arg.Dep.t

val resolve_targets_mixed :
     log:Log.t
  -> Common.t
  -> Dune.Main.build_system
  -> resolve_input list
  -> (t list, Arg.Dep.t * User_message.Style.t Pp.t list) result list

val resolve_targets_exn :
  log:Log.t -> Common.t -> Dune.Main.build_system -> Arg.Dep.t list -> t list
