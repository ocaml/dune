open Import

(** Generate a lockdir for a dev tool, build the dev tool, then run the dev
    tool. If a step is unnecessary then it is skipped. This function does not
    return, but starts running the dev tool in place of the current process. *)
val lock_build_and_run_dev_tool
  :  common:Common.t
  -> config:Dune_config_file.Dune_config.t
  -> Common.Builder.t
  -> Dune_pkg.Dev_tool.t
  -> args:string list
  -> 'a

val which_command : Dune_pkg.Dev_tool.t -> unit Cmd.t
val install_command : Dune_pkg.Dev_tool.t -> unit Cmd.t
val exec_command : Dune_pkg.Dev_tool.t -> unit Cmd.t
val env_command : unit Cmd.t
