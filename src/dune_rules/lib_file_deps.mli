open Import

module Group : sig
  type ocaml =
    | Cmi
    | Cmx

  type t =
    | Ocaml of ocaml
    | Melange of Melange.Cm_kind.t
    | Header
end

(** [deps t libs ~files] returns a list of path dependencies for all the files
    with extension [files] of libraries [libs]. *)
val deps : Lib.t list -> groups:Group.t list -> Dep.Set.t

(** [deps_of_entries ~opaque ~cm_kind libs] computes the file dependencies
    (glob deps on .cmi/.cmx files) for the given libraries. *)
val deps_of_entries : opaque:bool -> cm_kind:Lib_mode.Cm_kind.t -> Lib.t list -> Dep.Set.t

(** Specific-file deps on the [modules] of [lib]. Only valid for
    local libraries (where [Module.t] values are available). *)
val deps_of_entry_modules
  :  opaque:bool
  -> cm_kind:Lib_mode.Cm_kind.t
  -> Lib.t
  -> Module.t list
  -> Dep.Set.t

module Lib_index : sig
  type t

  val empty : t

  (** Third tuple element is [Some m] for local + unwrapped libs
      (with the entry's [Module.t]) and [None] otherwise (wrapped
      locals, externals). [no_ocamldep] names local libs whose [.d]
      files don't exist (short-circuited by [Dep_rules.skip_ocamldep]);
      the cross-library walk skips them. *)
  val create
    :  no_ocamldep:Lib.Set.t
    -> (Module_name.t * Lib.t * Module.t option) list
    -> t

  type classified =
    { tight : Module.t list Lib.Map.t
      (** Tight-eligible libs mapped to the entry modules referenced.
        Candidates for [deps_of_entry_modules]. *)
    ; non_tight : Lib.t list
      (** Wrapped locals, externals, or anything else; glob fallback.
        Sorted by [Lib.compare]. *)
    }

  (** Classify the libraries whose entry modules appear in
      [referenced_modules]. *)
  val filter_libs_with_modules : t -> referenced_modules:Module_name.Set.t -> classified

  (** Like [filter_libs_with_modules] but returns only the tight part. *)
  val tight_modules_per_lib
    :  t
    -> referenced_modules:Module_name.Set.t
    -> Module.t list Lib.Map.t

  (** [(lib, entry module)] pairs for the cross-library walk;
      excludes [no_ocamldep] libs and non-tight-eligible entries. *)
  val lookup_tight_entries : t -> Module_name.t -> (Lib.t * Module.t) list

  (** True for local + unwrapped libs whose entries all carry a
      [Module.t]. Used to drop unreached libs from a consumer's
      compile deps (the link rule still pulls them in). *)
  val is_tight_eligible : t -> Lib.t -> bool

  (** Local wrapped libs whose entry name is in [referenced_modules].
      The consumer must glob the wrapped lib's [Lib.closure] (see
      the file-level comment for why). *)
  val wrapped_libs_referenced : t -> referenced_modules:Module_name.Set.t -> Lib.Set.t
end

type path_specification =
  | Allow_all
  | Disallow_external of Lib_name.t

val raise_disallowed_external_path : loc:Loc.t -> Lib_name.t -> Path.t -> 'a

val eval
  :  loc:Loc.t
  -> expander:Expander.t
  -> paths:path_specification
  -> Dep_conf.t list
  -> Path.Set.t Memo.t
