open Dune_config_file
open Stdune

type t

val x : t -> Dune_engine.Context_name.t option
val capture_outputs : t -> bool
val root : t -> Workspace_root.t

val rpc
  :  t
  -> [ `Allow of Dune_lang.Dep_conf.t Dune_rpc_impl.Server.t
       (** Will run rpc if in watch mode and acquire the build lock *)
     | `Forbid_builds (** Promise not to build anything. For now, this isn't checked *)
     ]

val watch_exclusions : t -> string list
val stats : t -> Dune_stats.t option
val print_metrics : t -> bool
val dump_memo_graph_file : t -> Path.External.t option
val dump_memo_graph_format : t -> Dune_graph.Graph.File_format.t
val dump_memo_graph_with_timing : t -> bool
val watch : t -> Dune_rpc_impl.Watch_mode_config.t
val file_watcher : t -> Dune_engine.Scheduler.Run.file_watcher
val prefix_target : t -> string -> string

module Action_runner : sig
  type t =
    | No
    | Yes of
        (Dune_lang.Dep_conf.t Dune_rpc_impl.Server.t
         -> (Dune_engine.Action_exec.input -> Dune_engine.Action_runner.t option) Staged.t)
end

(** [Builder] describes how to initialize Dune. *)
module Builder : sig
  type t

  val set_root : t -> string -> t
  val forbid_builds : t -> t
  val set_default_root_is_cwd : t -> bool -> t
  val set_action_runner : t -> Action_runner.t -> t
  val set_log_file : t -> Dune_util.Log.File.t -> t
  val disable_log_file : t -> t
  val set_promote : t -> Dune_engine.Clflags.Promote.t -> t
  val default_target : t -> Arg.Dep.t
  val term : t Cmdliner.Term.t
end

(** [init] creates a [Common.t] by executing a sequence of side-effecting actions to
    initialize Dune's working environment based on the options determined in the\
    [Builder.t].

    Return the [Common.t] and the final configuration, which is the same as the one
    returned in the [config] field of [Dune_rules.Workspace.workspace ()]) *)
val init : Builder.t -> t * Dune_config_file.Dune_config.t

(** [examples [("description", "dune cmd foo"); ...]] is an [EXAMPLES] manpage
    section of enumerated examples illustrating how to run the documented
    commands. *)
val examples : (string * string) list -> Cmdliner.Manpage.block

(** [command_synopsis subcommands] is a custom [SYNOPSIS] manpage section
    listing the given [subcommands]. Each subcommand is prefixed with the `dune`
    top-level command. *)
val command_synopsis : string list -> Cmdliner.Manpage.block list

val help_secs : Cmdliner.Manpage.block list
val footer : Cmdliner.Manpage.block
val envs : Cmdliner.Cmd.Env.info list
val debug_backtraces : bool Cmdliner.Term.t
val config_from_config_file : Dune_config.Partial.t Cmdliner.Term.t
val display_term : Dune_config.Display.t option Cmdliner.Term.t
val context_arg : doc:string -> Dune_engine.Context_name.t Cmdliner.Term.t

(** A [--build-info] command line argument that print build information
    (included in [term]) *)
val build_info : unit Cmdliner.Term.t

val default_build_dir : string

module Let_syntax : sig
  val ( let+ ) : 'a Cmdliner.Term.t -> ('a -> 'b) -> 'b Cmdliner.Term.t
  val ( and+ ) : 'a Cmdliner.Term.t -> 'b Cmdliner.Term.t -> ('a * 'b) Cmdliner.Term.t
end

(** [one_of term1 term2] allows options from [term1] or exclusively options from
    [term2]. If the user passes options from both terms, an error is reported. *)
val one_of : 'a Cmdliner.Term.t -> 'a Cmdliner.Term.t -> 'a Cmdliner.Term.t
