(** An augmented context *)

(** A context augmented with: a lib-db, ... Super context are used for
    generating rules. *)

open! Stdune
open Import

type t

val to_dyn : t -> Dyn.t

val create :
     context:Context.t
  -> ?host:t
  -> projects:Dune_project.t list
  -> packages:Package.t Package.Name.Map.t
  -> stanzas:Dune_load.Dune_file.t list
  -> t

val context : t -> Context.t

val stanzas : t -> Dune_file.Stanzas.t Dir_with_dune.t list

val stanzas_in :
  t -> dir:Path.Build.t -> Dune_file.Stanzas.t Dir_with_dune.t option

val packages : t -> Package.t Package.Name.Map.t

val artifacts : t -> Artifacts.t

val build_dir : t -> Path.Build.t

val profile : t -> Profile.t

val host : t -> t

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
val ocaml_flags :
  t -> dir:Path.Build.t -> Dune_file.Buildable.t -> Ocaml_flags.t

val foreign_flags :
     t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> language:Foreign.Language.t
  -> string list Build.t

val menhir_flags :
     t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> string list Build.t

(** Binaries that are symlinked in the associated .bin directory of [dir]. This
    associated directory is [Path.relative dir ".bin"] *)
val local_binaries : t -> dir:Path.Build.t -> File_binding.Expanded.t list

(** odoc config in the corresponding [(env)] stanza. *)
val odoc : t -> dir:Path.Build.t -> Env_node.Odoc.t

(** Dump a directory environment in a readable form *)
val dump_env : t -> dir:Path.Build.t -> Dune_lang.t list Build.t

val find_scope_by_dir : t -> Path.Build.t -> Scope.t

val find_scope_by_project : t -> Dune_project.t -> Scope.t

val find_project_by_key : t -> Dune_project.File_key.t -> Dune_project.t

val add_rule :
     t
  -> ?sandbox:Sandbox_config.t
  -> ?mode:Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.t Build.With_targets.t
  -> unit

val add_rule_get_targets :
     t
  -> ?sandbox:Sandbox_config.t
  -> ?mode:Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.t Build.With_targets.t
  -> Path.Build.Set.t

val add_rules :
     t
  -> ?sandbox:Sandbox_config.t
  -> dir:Path.Build.t
  -> Action.t Build.With_targets.t list
  -> unit

val add_alias_action :
     t
  -> Build_system.Alias.t
  -> dir:Path.Build.t
  -> loc:Loc.t option
  -> ?locks:Path.t list
  -> stamp:_
  -> Action.t Build.With_targets.t
  -> unit

(** [resolve_program t ?hint name] resolves a program. [name] is looked up in
    the workspace, if it is not found in the tree is is looked up in the PATH.
    If it is not found at all, the resulting [Action.Prog.t] will either return
    the resolved path or a record with details about the error and possibly a
    hint.

    [hint] should tell the user what to install when the program is not found. *)
val resolve_program :
     t
  -> dir:Path.Build.t
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> Action.Prog.t

val expander : t -> dir:Path.Build.t -> Expander.t

val dir_status_db : t -> Dir_status.DB.t

module As_memo_key : sig
  type nonrec t = t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int
end
