type t

val workspace_file : t -> Arg.Path.t option

val x : t -> Dune_engine.Context_name.t option

val profile : t -> Dune_rules.Profile.t option

val capture_outputs : t -> bool

val root : t -> Workspace_root.t

val config : t -> Dune_config.t

val rpc : t -> Dune_rpc_impl.Server.t option

val set_config : t -> Dune_config.t -> t

val stats : t -> Chrome_trace.t option

module Only_packages : sig
  type t = private
    { names : Dune_engine.Package.Name.Set.t
    ; command_line_option : string
          (** Which of [-p], [--only-packages], ... was passed *)
    }
end

val only_packages : t -> Only_packages.t option

val watch : t -> bool

val default_target : t -> Arg.Dep.t

val prefix_target : t -> string -> string

val instrument_with : t -> Dune_engine.Lib_name.t list option

(** [set_common] executes sequence of side-effecting actions to initialize
    Dune's working environment based on the options determined in a [Common.t]
    record.contents. *)
val set_common : ?log_file:Dune_util.Log.File.t -> t -> unit

(** [examples \[("description", "dune cmd foo"); ...\]] is an [EXAMPLES] manpage
    section of enumerated examples illustrating how to run the documented
    commands. *)
val examples : (string * string) list -> Cmdliner.Manpage.block

(** [command_syposis subcommands] is a custom [SYNOPSIS] manpage section listing
    the given [subcommands]. Each subcommand is prefixed with the `dune`
    top-level command. *)
val command_synopsis : string list -> Cmdliner.Manpage.block list

val help_secs : Cmdliner.Manpage.block list

val footer : Cmdliner.Manpage.block

val term : t Cmdliner.Term.t

(** Set whether Dune should print the "Entering directory '<dir>'" message *)
val set_print_directory : t -> bool -> t

val debug_backtraces : bool Cmdliner.Term.t

val config_term : Dune_config.t Cmdliner.Term.t

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

val set_rpc : t -> Dune_rpc_impl.Server.t -> t
