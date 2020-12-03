module Context_name = Dune_engine.Context_name
module Build_context = Dune_engine.Build_context
module Action = Dune_engine.Action
module Dir_set = Dune_engine.Dir_set
module Process = Dune_engine.Process
module Action_dune_lang = Dune_engine.Action_dune_lang
module Dpath = Dune_engine.Dpath
module Action_exec = Dune_engine.Action_exec
module Dune_project = Dune_engine.Dune_project
module Response_file = Dune_engine.Response_file
module Action_mapper = Dune_engine.Action_mapper
module File_selector = Dune_engine.File_selector
module Rule = Dune_engine.Rule
module File_tree = Dune_engine.File_tree
module Rules = Dune_engine.Rules
module Foreign_language = Dune_engine.Foreign_language
module Sandbox_config = Dune_engine.Sandbox_config
module Alias = Dune_engine.Alias
module Format_config = Dune_engine.Format_config
module Sandbox_mode = Dune_engine.Sandbox_mode
module Artifact_substitution = Dune_engine.Artifact_substitution
module Glob = Dune_engine.Glob
module Scheduler = Dune_engine.Scheduler
module Hooks = Dune_engine.Hooks
module Section = Dune_engine.Section
module Build = Dune_engine.Build
module Import = Dune_engine.Import
module Stanza = Dune_engine.Stanza
module Build_system = Dune_engine.Build_system
module Include_stanza = Dune_engine.Include_stanza
module Static_deps = Dune_engine.Static_deps
module Cached_digest = Dune_engine.Cached_digest
module Install = Dune_engine.Install
module Stats = Dune_engine.Stats
module Clflags = Dune_engine.Clflags
module Lib_name = Dune_engine.Lib_name
module Config = Dune_engine.Config
module Ml_kind = Dune_engine.Ml_kind
module Opam_file = Dune_engine.Opam_file
module String_with_vars = Dune_engine.String_with_vars
module Cram_test = Dune_engine.Cram_test
module Package = Dune_engine.Package
module Sub_dirs = Dune_engine.Sub_dirs
module Dep = Dune_engine.Dep
module Persistent = Dune_engine.Persistent
module Utils = Dune_engine.Utils
module Dep_path = Dune_engine.Dep_path
module Predicate_lang = Dune_engine.Predicate_lang
module Value = Dune_engine.Value
module Dialect = Dune_engine.Dialect
module Predicate = Dune_engine.Predicate
module Variant = Dune_engine.Variant
module Diff = Dune_engine.Diff
module Print_diff = Dune_engine.Print_diff
module Vcs = Dune_engine.Vcs
module Dune_lexer = Dune_engine.Dune_lexer
module Format_dune_lang = Dune_engine.Format_dune_lang
module Action_inputs = Dune_engine.Action.Inputs
module Action_outputs = Dune_engine.Action.Outputs

module type Action_ast = sig
  type program

  type path

  type target

  type string

  type t =
    | Run of program * string list
    | With_accepted_exit_codes of int Predicate_lang.t * t
    | Dynamic_run of program * string list
    | Chdir of path * t
    | Setenv of string * string * t
    (* It's not possible to use a build path here since jbuild supports
       redirecting to /dev/null. In [dune] files this is replaced with %{null} *)
    | Redirect_out of Action.Outputs.t * target * t
    | Redirect_in of Action.Inputs.t * path * t
    | Ignore of Action.Outputs.t * t
    | Progn of t list
    | Echo of string list
    | Cat of path
    | Copy of path * target
    | Symlink of path * target
    | Copy_and_add_line_directive of path * target
    | System of string
    | Bash of string
    | Write_file of target * string
    | Rename of target * target
    | Remove_tree of target
    | Mkdir of path
    | Digest_files of path list
    | Diff of (path, target) Diff.t
    | Merge_files_into of path list * string list * target
    | No_infer of t
    | Pipe of Dune_engine.Action_intf.Outputs.t * t list
    | Format_dune_file of Dune_lang.Syntax.Version.t * path * target
    | Cram of path
end

module type Action_helpers = sig
  type program

  type path

  type target

  type string

  type t

  val run : program -> string list -> t

  val chdir : path -> t -> t

  val setenv : string -> string -> t -> t

  val with_stdout_to : target -> t -> t

  val with_stderr_to : target -> t -> t

  val with_outputs_to : target -> t -> t

  val with_stdin_from : path -> t -> t

  val ignore_stdout : t -> t

  val ignore_stderr : t -> t

  val ignore_outputs : t -> t

  val progn : t list -> t

  val echo : string list -> t

  val cat : path -> t

  val copy : path -> target -> t

  val symlink : path -> target -> t

  val copy_and_add_line_directive : path -> target -> t

  val system : string -> t

  val bash : string -> t

  val write_file : target -> string -> t

  val rename : target -> target -> t

  val remove_tree : target -> t

  val mkdir : path -> t

  val digest_files : path list -> t

  val diff : ?optional:bool -> ?mode:Diff.Mode.t -> path -> target -> t

  val format_dune_file :
    version:Dune_lang.Syntax.Version.t -> path -> target -> t
end

module Action_plugin = Dune_engine.Action_plugin
module Report_error = Dune_engine.Report_error
module Promotion = Dune_engine.Promotion
module Dtemp = Dune_engine.Dtemp
