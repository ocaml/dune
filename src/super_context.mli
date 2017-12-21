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
  -> aliases:Alias.Store.t
  -> scopes:Scope.t list
  -> file_tree:File_tree.t
  -> packages:Package.t String_map.t
  -> stanzas:(Path.t * Scope.t * Stanzas.t) list
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

val expand_vars : t -> scope:Scope.t -> dir:Path.t -> String_with_vars.t -> string

val add_rule
  :  t
  -> ?sandbox:bool
  -> ?fallback:Jbuild.Rule.Fallback.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> (unit, Action.t) Build.t
  -> unit
val add_rules
  :  t
  -> ?sandbox:bool
  -> (unit, Action.t) Build.t list
  -> unit
val rules : t -> Build_interpret.Rule.t list

val sources_and_targets_known_so_far : t -> src_path:Path.t -> String_set.t

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

(** Unique name, even for internal libraries *)
val unique_library_name : t -> Lib.t -> string

module Libs : sig
  val find : t -> from:Path.t -> string -> Lib.t option

  val load_requires     : t -> dir:Path.t -> item:string -> (unit, Lib.t list) Build.t
  val load_runtime_deps : t -> dir:Path.t -> item:string -> (unit, Lib.t list) Build.t

  val lib_is_available : t -> from:Path.t -> string -> bool

  (** Add rules for (select ...) forms *)
  val add_select_rules : t -> dir:Path.t -> Lib_deps.t -> unit

  (** Returns the closed list of dependencies for a dependency list in
     a stanza. The second arrow is the same as the first one but with
     an added dependency on the .merlin if [has_dot_merlin] is
     [true]. *)
  val requires
    :  t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> item:string (* Library name or first exe name *)
    -> libraries:Lib_deps.t
    -> preprocess:Preprocess_map.t
    -> virtual_deps:string list
    -> has_dot_merlin:bool
    -> (unit, Lib.t list) Build.t * (unit, Lib.t list) Build.t

  (** Setup the rules for ppx runtime dependencies *)
  val setup_runtime_deps
    :  t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> item:string (* Library name or first exe name *)
    -> libraries:Lib_deps.t
    -> ppx_runtime_libraries:string list
    -> unit

  (** [file_deps ~ext] is an arrow that record dependencies on all the files with
      extension [ext] of the libraries given as input. *)
  val file_deps : t -> ext:string -> (Lib.t list, Lib.t list) Build.t

  (** Same as [file_deps] but for a single known library *)
  val static_file_deps : ext:string -> Lib.Internal.t -> ('a, 'a) Build.t

  (** Setup the alias that depends on all files with a given extension for a library *)
  val setup_file_deps_alias : t -> Lib.Internal.t -> ext:string -> Path.t list -> unit

  (** Setup an alias that depend on all files with the given extensions.

      To depend on this alias, use [~ext:"ext1-and-ext2-...-extn"]
  *)
  val setup_file_deps_group_alias : t -> Lib.Internal.t -> exts:string list -> unit
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

  (** The arrow takes as input the list of actual dependencies *)
  val run
    :  t
    -> Action.Unexpanded.t
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> targets:targets
    -> scope:Scope.t
    -> (Path.t list, Action.t) Build.t
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
    -> scope:Scope.t
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
  :  t
  -> scope:Scope.t
  -> dir:Path.t
  -> Ordered_set_lang.Unexpanded.t
  -> standard:string list
  -> (unit, string list) Build.t

module Pkg_version : sig
  val set : t -> Package.t -> (unit, string option) Build.t -> (unit, string option) Build.t
end
