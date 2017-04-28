(** A augmanted context *)

(** A context augmented with: a lib-db, ...

    Super context are used for generating rules.
*)

open Import
open Jbuild_types

(** A directory with a jbuild *)
module Dir_with_jbuild : sig
  type t =
    { src_dir : Path.t
    ; ctx_dir : Path.t (** [_build/context-name/src_dir] *)
    ; stanzas : Stanzas.t
    }
end

type t

val create
  :  context:Context.t
  -> aliases:Alias.Store.t
  -> dirs_with_dot_opam_files:Path.Set.t
  -> file_tree:File_tree.t
  -> packages:Package.t String_map.t
  -> stanzas:(Path.t * Stanzas.t) list
  -> filter_out_optional_stanzas_with_missing_deps:bool
  -> t

val context   : t -> Context.t
val aliases   : t -> Alias.Store.t
val stanzas   : t -> Dir_with_jbuild.t list
val packages  : t -> Package.t String_map.t
val file_tree : t -> File_tree.t
val artifacts : t -> Artifacts.t
val stanzas_to_consider_for_install : t -> (Path.t * Stanza.t) list
val cxx_flags : t -> string list

val expand_var_no_root : t -> string -> string option
val expand_vars : t -> dir:Path.t -> String_with_vars.t -> string

val add_rule : t -> ?sandbox:bool -> (unit, Action.t) Build.t -> unit
val rules : t -> Build_interpret.Rule.t list

val sources_and_targets_known_so_far : t -> src_path:Path.t -> String_set.t

module Libs : sig
  val find : t -> from:Path.t -> string -> Lib.t option
  val vrequires : t -> dir:Path.t -> item:string ->  Lib.t list Build.Vspec.t
  val load_requires : t -> dir:Path.t -> item:string -> (unit, Lib.t list) Build.t

  val vruntime_deps : t -> dir:Path.t -> item:string ->  Lib.t list Build.Vspec.t
  val load_runtime_deps : t -> dir:Path.t -> item:string -> (unit, Lib.t list) Build.t

  val closure
    :  t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> Lib_deps.t
    -> (unit, Lib.t list) Build.t

  val closed_ppx_runtime_deps_of
    :  t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> Lib_deps.t
    -> (unit, Lib.t list) Build.t

  val lib_is_available : t -> from:Path.t -> string -> bool

  val add_select_rules : t -> dir:Path.t -> Lib_deps.t -> unit
end

(** Interpret dependencies written in jbuild files *)
module Deps : sig
  val interpret : t -> dir:Path.t -> Dep_conf.t list -> (unit, unit) Build.t

  (** Interpret plain dependencies, replacing other (glob_files, files_recursively_in,
      ...) by None *)
  val only_plain_files : t -> dir:Path.t -> Dep_conf.t list -> Path.t option list
end
