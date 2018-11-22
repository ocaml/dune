(** A augmanted context *)

(** A context augmented with: a lib-db, ...

    Super context are used for generating rules.
*)

open! Stdune
open Import
open Dune_file

type t

val create
  :  context:Context.t
  -> ?host:t
  -> projects:Dune_project.t list
  -> file_tree:File_tree.t
  -> packages:Package.t Package.Name.Map.t
  -> stanzas:Dune_load.Dune_file.t list
  -> external_lib_deps_mode:bool
  -> build_system:Build_system.t
  -> t

val context   : t -> Context.t
val stanzas   : t -> Stanzas.t Dir_with_dune.t list
val stanzas_in : t -> dir:Path.t -> Stanzas.t Dir_with_dune.t option
val packages  : t -> Package.t Package.Name.Map.t
val libs_by_package : t -> (Package.t * Lib.Set.t) Package.Name.Map.t
val file_tree : t -> File_tree.t
val artifacts : t -> Artifacts.t
val cxx_flags : t -> string list
val build_dir : t -> Path.t
val profile   : t -> string
val host : t -> t
val build_system : t -> Build_system.t
val external_lib_deps_mode : t -> bool

(** All public libraries of the workspace *)
val public_libs : t -> Lib.DB.t

(** Installed libraries that are not part of the workspace *)
val installed_libs : t -> Lib.DB.t

(** All non-public library names *)
val internal_lib_names : t -> Lib_name.Set.t

(** Compute the ocaml flags based on the directory environment and a
    buildable stanza *)
val ocaml_flags
  :  t
  -> dir:Path.t
  -> Buildable.t
  -> Ocaml_flags.t

val file_bindings : t -> dir:Path.t -> string File_bindings.t

(** Dump a directory environment in a readable form *)
val dump_env : t -> dir:Path.t -> (unit, Dune_lang.t list) Build.t

val find_scope_by_dir  : t -> Path.t              -> Scope.t
val find_scope_by_name : t -> Dune_project.Name.t -> Scope.t

val rule_context : t -> dir:Path.t -> Rule_context.t

(** See [Build_system for details] *)
val eval_glob : t -> dir:Path.t -> Re.re -> string list
val load_dir : t -> dir:Path.t -> unit
val on_load_dir : t -> dir:Path.t -> f:(unit -> unit) -> unit

val source_files : t -> src_path:Path.t -> String.Set.t

(** [prog_spec t ?hint name] resolve a program. [name] is looked up in the
    workspace, if it is not found in the tree is is looked up in the PATH. If it
    is not found at all, the resulting [Prog_spec.t] will either return the
    resolved path or a record with details about the error and possibly a hint.

    [hint] should tell the user what to install when the program is not found.
*)
val resolve_program
  :  t
  -> dir:Path.t
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> Action.Prog.t

(** Interpret action written in jbuild files *)
module Action : sig
  (** The arrow takes as input the list of actual dependencies *)
  val run
    :  t
    -> loc:Loc.t
    -> dir:Path.t
    -> expander:Expander.t
    -> dep_kind:Lib_deps_info.Kind.t
    -> targets:Expander.targets
    -> targets_dir:Path.t
    -> Action_unexpanded.t
    -> (Path.t Bindings.t, Action.t) Build.t

  val map_exe : t -> Path.t -> Path.t
end

module Pkg_version : sig
  val set : t -> Package.t -> (unit, string option) Build.t -> (unit, string option) Build.t
end

module Scope_key : sig
  val of_string : t -> string -> string * Lib.DB.t

  val to_string : string -> Dune_project.Name.t -> string
end

val opaque : t -> bool

val expander : t -> dir:Path.t -> Expander.t
