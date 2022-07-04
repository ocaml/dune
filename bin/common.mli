type t

val capture_outputs : t -> bool

val root : t -> Workspace_root.t

val rpc : t -> Dune_rpc_impl.Server.t

val stats : t -> Dune_stats.t option

val print_metrics : t -> bool

val dump_memo_graph_file : t -> string option

val dump_memo_graph_format : t -> Dune_graph.Graph.File_format.t

val dump_memo_graph_with_timing : t -> bool

val watch : t -> Watch_mode_config.t

val file_watcher : t -> Dune_engine.Scheduler.Run.file_watcher

val default_target : t -> Arg.Dep.t

val prefix_target : t -> string -> string

(** [init] executes sequence of side-effecting actions to initialize Dune's
    working environment based on the options determined in a [Common.t]
    record.contents.

    Return the final configuration, which is the same as the one returned in the
    [config] field of [Dune_rules.Workspace.workspace ()]) *)
val init : ?log_file:Dune_util.Log.File.t -> t -> Dune_config.t

(** [examples \[("description", "dune cmd foo"); ...\]] is an [EXAMPLES] manpage
    section of enumerated examples illustrating how to run the documented
    commands. *)
val examples : (string * string) list -> Cmdliner.Manpage.block

(** [command_synopsis subcommands] is a custom [SYNOPSIS] manpage section
    listing the given [subcommands]. Each subcommand is prefixed with the `dune`
    top-level command. *)
val command_synopsis : string list -> Cmdliner.Manpage.block list

val help_secs : Cmdliner.Manpage.block list

val footer : Cmdliner.Manpage.block

val term : t Cmdliner.Term.t

val term_with_default_root_is_cwd : t Cmdliner.Term.t

(** Set whether Dune should print the "Entering directory '<dir>'" message *)
val set_print_directory : t -> bool -> t

val set_promote : t -> Dune_engine.Clflags.Promote.t -> t

val debug_backtraces : bool Cmdliner.Term.t

val config_from_config_file : Dune_config.Partial.t Cmdliner.Term.t

val display_term : Dune_engine.Scheduler.Config.Display.t option Cmdliner.Term.t

val context_arg : doc:string -> Dune_engine.Context_name.t Cmdliner.Term.t

(** A [--build-info] command line argument that print build information
    (included in [term]) *)
val build_info : unit Cmdliner.Term.t

val default_build_dir : string

module Let_syntax : sig
  val ( let+ ) : 'a Cmdliner.Term.t -> ('a -> 'b) -> 'b Cmdliner.Term.t

  val ( and+ ) :
    'a Cmdliner.Term.t -> 'b Cmdliner.Term.t -> ('a * 'b) Cmdliner.Term.t
end
