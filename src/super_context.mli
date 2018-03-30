(** A augmanted context *)

(** A context augmented with: a lib-db, ...

    Super context are used for generating rules.
*)

open Import
open Jbuild

(** A directory with a jbuild *)
module Dir_with_jbuild : sig
  type t =
    { src_dir : Path.t
    ; ctx_dir : Path.t (** [_build/context-name/src_dir] *)
    ; stanzas : Stanzas.t
    ; scope   : Scope.t
    }
end

type t

val create
  :  context:Context.t
  -> ?host:t
  -> scopes:Scope_info.t list
  -> file_tree:File_tree.t
  -> packages:Package.t Package.Name.Map.t
  -> stanzas:(Path.t * Scope_info.t * Stanzas.t) list
  -> filter_out_optional_stanzas_with_missing_deps:bool
  -> build_system:Build_system.t
  -> t

val context   : t -> Context.t
val stanzas   : t -> Dir_with_jbuild.t list
val packages  : t -> Package.t Package.Name.Map.t
val libs_by_package : t -> (Package.t * Lib.Set.t) Package.Name.Map.t
val file_tree : t -> File_tree.t
val artifacts : t -> Artifacts.t
val stanzas_to_consider_for_install : t -> (Path.t * Scope.t * Stanza.t) list
val cxx_flags : t -> string list
val build_dir : t -> Path.t
val host : t -> t
val build_system : t -> Build_system.t

(** All public libraries of the workspace *)
val public_libs : t -> Lib.DB.t

(** Installed libraries that are not part of the workspace *)
val installed_libs : t -> Lib.DB.t

val find_scope_by_dir  : t -> Path.t        -> Scope.t
val find_scope_by_name : t -> string option -> Scope.t

val expand_vars
  :  t
  -> scope:Scope.t
  -> dir:Path.t
  -> ?extra_vars:Action.Var_expansion.t String_map.t
  -> String_with_vars.t
  -> string

val expand_and_eval_set
  :  t
  -> scope:Scope.t
  -> dir:Path.t
  -> ?extra_vars:Action.Var_expansion.t String_map.t
  -> Ordered_set_lang.Unexpanded.t
  -> standard:string list
  -> (unit, string list) Build.t

val prefix_rules
  : t
  -> (unit, unit) Build.t
  -> f:(unit -> 'a)
  -> 'a
val add_rule
  :  t
  -> ?sandbox:bool
  -> ?mode:Jbuild.Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> (unit, Action.t) Build.t
  -> unit
val add_rule_get_targets
  :  t
  -> ?sandbox:bool
  -> ?mode:Jbuild.Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> (unit, Action.t) Build.t
  -> Path.t list
val add_rules
  :  t
  -> ?sandbox:bool
  -> (unit, Action.t) Build.t list
  -> unit
val add_alias_deps
  :  t
  -> Build_system.Alias.t
  -> ?dyn_deps:(unit, Path.Set.t) Build.t
  -> Path.Set.t
  -> unit
val add_alias_action
  :  t
  -> Build_system.Alias.t
  -> ?locks:Path.t list
  -> stamp:Sexp.t
  -> (unit, Action.t) Build.t
  -> unit

(** See [Build_system for details] *)
val eval_glob : t -> dir:Path.t -> Re.re -> string list
val load_dir : t -> dir:Path.t -> unit
val on_load_dir : t -> dir:Path.t -> f:(unit -> unit) -> unit

val source_files : t -> src_path:Path.t -> String_set.t

(** [prog_spec t ?hint name] resolve a program. [name] is looked up in the
    workspace, if it is not found in the tree is is looked up in the PATH. If it
    is not found at all, the resulting [Prog_spec.t] will either return the
    resolved path or a record with details about the error and possibly a hint.

    [hint] should tell the user what to install when the program is not found.
*)
val resolve_program
  :  t
  -> ?hint:string
  -> string
  -> Action.Prog.t

module Libs : sig
  (** Make sure all rules produces by [f] record the library
      dependencies for [jbuilder external-lib-deps] and depend on the
      generation of the .merlin file. *)
  val with_lib_deps
    :  t
    -> Lib.Compile.t
    -> dir:Path.t
    -> has_dot_merlin:bool
    -> f:((unit, Lib.L.t) Build.t -> 'a)
    -> 'a

  (** Generate the rules for the [(select ...)] forms in library dependencies *)
  val gen_select_rules : t -> dir:Path.t -> Lib.Compile.t -> unit

  (** [file_deps ~ext] is an arrow that record dependencies on all the
      files with extension [ext] of the libraries given as input. *)
  val file_deps : t -> ext:string -> (Lib.t list, Lib.t list) Build.t

  (** Setup the alias that depends on all files with a given extension
      for a library *)
  val setup_file_deps_alias
    :  t
    -> dir:Path.t
    -> ext:string
    -> Library.t
    -> Path.Set.t
    -> unit

  (** Setup an alias that depend on all files with the given extensions.

      To depend on this alias, use [~ext:"ext1-and-ext2-...-extn"]
  *)
  val setup_file_deps_group_alias
    :  t
    -> dir:Path.t
    -> exts:string list
    -> Library.t
    -> unit
end

(** Interpret dependencies written in jbuild files *)
module Deps : sig
  (** Evaluates to the actual list of dependencies, ignoring aliases *)
  val interpret
    :  t
    -> scope:Scope.t
    -> dir:Path.t
    -> Dep_conf.t list
    -> (unit, Path.t list) Build.t
end

(** Interpret action written in jbuild files *)
module Action : sig
  type targets =
    | Static of Path.t list
    | Infer
    | Alias (** This action is for an alias *)

  (** The arrow takes as input the list of actual dependencies *)
  val run
    :  t
    -> ?extra_vars:Action.Var_expansion.t String_map.t
    -> Action.Unexpanded.t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> targets:targets
    -> scope:Scope.t
    -> (Path.t list, Action.t) Build.t
end

module Pkg_version : sig
  val set : t -> Package.t -> (unit, string option) Build.t -> (unit, string option) Build.t
end

module Scope_key : sig
  val of_string : t -> string -> string * Lib.DB.t

  val to_string : string -> Scope_info.Name.t -> string
end
