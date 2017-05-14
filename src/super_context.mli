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
    ; pkgs    : Pkgs.t
    }
end

type t

val create
  :  context:Context.t
  -> aliases:Alias.Store.t
  -> dirs_with_dot_opam_files:Path.Set.t
  -> file_tree:File_tree.t
  -> packages:Package.t String_map.t
  -> stanzas:(Path.t * Pkgs.t * Stanzas.t) list
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

val add_rule : t -> ?sandbox:bool -> targets:Path.t list -> (unit, Action.t) Build.t -> unit
val rules : t -> Build_interpret.Rule.t list

val sources_and_targets_known_so_far : t -> src_path:Path.t -> String_set.t

(** [prog_spec t ?hint ?in_the_tree name] resolve a program. If [in_the_tree] is [true]
    (the default), [name] is looked up in the workspace. Otherwise, or if it is not found
    in the tree is is looked up in the PATH. If it is not found at all, the resulting
    [Prog_spec.t] will fail when evaluated.

    [hint] should tell the user what to install when the program is not found.
*)
val resolve_program
  :  t
  -> ?hint:string
  -> ?in_the_tree:bool (* default true *)
  -> string
  -> _ Build.Prog_spec.t

module Libs : sig
  val find : t -> from:Path.t -> string -> Lib.t option

  val load_requires     : t -> dir:Path.t -> item:string -> (unit, Lib.t list) Build.t
  val load_runtime_deps : t -> dir:Path.t -> item:string -> (unit, Lib.t list) Build.t

  val lib_is_available : t -> from:Path.t -> string -> bool

  (** Add rules for (select ...) forms *)
  val add_select_rules : t -> dir:Path.t -> Lib_deps.t -> unit

  (** Returns the closed list of dependencies for a dependency list in a stanza. The
      second arrow is the same as the first one but with an added dependency on the
      .merlin. *)
  val requires
    :  t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> item:string (** Library name or first exe name *)
    -> libraries:Lib_deps.t
    -> preprocess:Preprocess_map.t
    -> virtual_deps:string list
    -> (unit, Lib.t list) Build.t * (unit, Lib.t list) Build.t

  (** Setup the rules for ppx runtime dependencies *)
  val setup_runtime_deps
    :  t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> item:string (** Library name or first exe name *)
    -> libraries:Lib_deps.t
    -> ppx_runtime_libraries:string list
    -> unit
end

(** Interpret dependencies written in jbuild files *)
module Deps : sig
  val interpret : t -> dir:Path.t -> Dep_conf.t list -> (unit, unit) Build.t

  (** Interpret plain dependencies, replacing other (glob_files, files_recursively_in,
      ...) by None *)
  val only_plain_files : t -> dir:Path.t -> Dep_conf.t list -> Path.t option list
end

(** Interpret action written in jbuild files *)
module Action : sig
  val run
    :  t
    -> Action.Mini_shexp.Unexpanded.t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> targets:Path.t list
    -> deps:Path.t option list
    -> package_context:Pkgs.t
    -> (unit, Action.t) Build.t
end

(** Preprocessing stuff *)
module PP : sig
  (** Setup pre-processing rules and return the list of pre-processed modules *)
  val pped_modules
    :  t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> modules:Module.t String_map.t
    -> preprocess:Preprocess_map.t
    -> preprocessor_deps:Dep_conf.t list
    -> lib_name:string option
    -> package_context:Pkgs.t
    -> Module.t String_map.t

  (** Get a path to a cached ppx driver *)
  val get_ppx_driver
    :  t
    -> Pp.t list
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> Path.t

  (** [cookie_library_name lib_name] is ["--cookie"; lib_name] if [lib_name] is not
      [None] *)
  val cookie_library_name : string option -> string list
end

val expand_and_eval_set
  :  dir:Path.t
  -> Ordered_set_lang.Unexpanded.t
  -> standard:string list
  -> (unit, string list) Build.t

module Pkg_version : sig
  val set : t -> Package.t -> (unit, string option) Build.t -> (unit, string option) Build.t
end
