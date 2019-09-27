(** An augmented context *)

(** A context augmented with: a lib-db, ...

    Super context are used for generating rules. *)

open! Stdune
open Import
open Dune_file

type t

val to_dyn : t -> Dyn.t

val create :
     context:Context.t
  -> ?host:t
  -> projects:Dune_project.t list
  -> packages:Package.t Package.Name.Map.t
  -> stanzas:Dune_load.Dune_file.t list
  -> external_lib_deps_mode:bool
  -> t

val context : t -> Context.t

val stanzas : t -> Stanzas.t Dir_with_dune.t list

val stanzas_in : t -> dir:Path.Build.t -> Stanzas.t Dir_with_dune.t option

val packages : t -> Package.t Package.Name.Map.t

val artifacts : t -> Artifacts.t

val build_dir : t -> Path.Build.t

val profile : t -> Profile.t

val host : t -> t

val external_lib_deps_mode : t -> bool

module Lib_entry : sig
  type t =
    | Library of Lib.Local.t
    | Deprecated_library_name of Dune_file.Deprecated_library_name.t
end

val lib_entries_of_package : t -> Package.Name.t -> Lib_entry.t list

(** All public libraries of the workspace *)
val public_libs : t -> Lib.DB.t

(** Installed libraries that are not part of the workspace *)
val installed_libs : t -> Lib.DB.t

(** All non-public library names *)
val internal_lib_names : t -> Lib_name.Set.t

(** Compute the ocaml flags based on the directory environment and a buildable
    stanza *)
val ocaml_flags : t -> dir:Path.Build.t -> Buildable.t -> Ocaml_flags.t

val c_flags :
     t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t C.Kind.Dict.t
  -> string list Build.t C.Kind.Dict.t

(** Binaries that are symlinked in the associated .bin directory of [dir]. This
    associated directory is [Path.relative dir ".bin"] *)
val local_binaries : t -> dir:Path.Build.t -> File_binding.Expanded.t list

(** Dump a directory environment in a readable form *)
val dump_env : t -> dir:Path.Build.t -> Dune_lang.t list Build.t

val find_scope_by_dir : t -> Path.Build.t -> Scope.t

val find_scope_by_project : t -> Dune_project.t -> Scope.t

val find_project_by_key : t -> Dune_project.File_key.t -> Dune_project.t

(** Tells whether the given source directory is marked as vendored *)
val dir_is_vendored : Path.Source.t -> bool

val add_rule :
     t
  -> ?sandbox:Sandbox_config.t
  -> ?mode:Dune_file.Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.t Build.t
  -> unit

val add_rule_get_targets :
     t
  -> ?sandbox:Sandbox_config.t
  -> ?mode:Dune_file.Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.t Build.t
  -> Path.Build.Set.t

val add_rules :
     t
  -> ?sandbox:Sandbox_config.t
  -> dir:Path.Build.t
  -> Action.t Build.t list
  -> unit

val add_alias_action :
     t
  -> Build_system.Alias.t
  -> dir:Path.Build.t
  -> loc:Loc.t option
  -> ?locks:Path.t list
  -> stamp:_
  -> Action.t Build.t
  -> unit

val source_files : src_path:Path.Source.t -> String.Set.t

(** [prog_spec t ?hint name] resolve a program. [name] is looked up in the
    workspace, if it is not found in the tree is is looked up in the PATH. If
    it is not found at all, the resulting [Prog_spec.t] will either return the
    resolved path or a record with details about the error and possibly a hint.

    [hint] should tell the user what to install when the program is not found. *)
val resolve_program :
     t
  -> dir:Path.Build.t
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> Action.Prog.t

module Libs : sig
  (** Make sure all rules produces by [f] record the library dependencies for
      [dune external-lib-deps] and depend on the generation of the .merlin
      file.

      /!\ WARNING /!\: make sure the last function call inside [f] is fully
      applied, otherwise the function might end up being executed after this
      function has returned. Consider addin a type annotation to make sure this
      doesn't happen by mistake. *)
  val with_lib_deps :
    t -> Lib.Compile.t -> dir:Path.Build.t -> f:(unit -> 'a) -> 'a

  (** Generate the rules for the [(select ...)] forms in library dependencies *)
  val gen_select_rules : t -> dir:Path.Build.t -> Lib.Compile.t -> unit
end

(** Interpret dependencies written in jbuild files *)
module Deps : sig
  (** Evaluates to the actual list of dependencies, ignoring aliases, and
      registers them as the action dependencies. *)
  val interpret : t -> expander:Expander.t -> Dep_conf.t list -> unit Build.t

  (** Evaluates to the actual list of dependencies, ignoring aliases, and
      registers them as the action dependencies.

      It returns bindings that are later used for action expansion. *)
  val interpret_named :
       t
    -> expander:Expander.t
    -> Dep_conf.t Bindings.t
    -> Path.t Bindings.t Build.t
end

(** Interpret action written in jbuild files *)
module Action : sig
  (** This function takes as input the list of dependencies written by user,
      which is used for action expansion. These must be registered with the
      build description before calling [run]. *)
  val run :
       t
    -> loc:Loc.t
    -> expander:Expander.t
    -> dep_kind:Lib_deps_info.Kind.t
    -> targets:Expander.Targets.t
    -> targets_dir:Path.Build.t
    -> Action_unexpanded.t
    -> Path.t Bindings.t Build.t
    -> Action.t Build.t

  val map_exe : t -> Path.t -> Path.t
end

val opaque : t -> bool

val expander : t -> dir:Path.Build.t -> Expander.t

val dir_status_db : t -> Dir_status.DB.t

module As_memo_key : sig
  type nonrec t = t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int
end
