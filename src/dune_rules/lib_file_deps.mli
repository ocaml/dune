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

(** [deps_of_entries ~opaque ~cm_kind libs] computes the file dependencies (glob
    deps on .cmi/.cmx files) for the given libraries. *)
val deps_of_entries : opaque:bool -> cm_kind:Lib_mode.Cm_kind.t -> Lib.t list -> Dep.Set.t

(** Specific-file deps on the [modules] of [lib]. Only valid for local libraries
    (where [Module.t] values are available).

    Currently produces complete per-module deps only for [cm_kind = Ocaml _]
    (cmi + cmx); for [Melange _] only the cmi is emitted — there is no
    per-module cmj arm, asymmetric with [deps_of_entries]. The sole caller
    ([Module_compilation.lib_deps_for_module]) gates Melange out before
    reaching this function, so this asymmetry is not observable today. If a
    future Melange caller is added, extend with a [want_cmj] arm. *)
val deps_of_entry_modules
  :  opaque:bool
  -> cm_kind:Lib_mode.Cm_kind.t
  -> Lib.t
  -> Module.t list
  -> Dep.Set.t

module Lib_index : sig
  type t

  (** Third tuple element is [Some m] for local + unwrapped libs (with the
      entry's [Module.t]) and [None] otherwise (wrapped locals, externals).
      [no_ocamldep] names local libs that are walker-terminal (singletons
      with no resolved requires) — the cross-library walk would gain nothing
      by running ocamldep on them, so it skips them. *)
  val create
    :  no_ocamldep:Lib.Set.t
    -> (Module_name.t * Lib.t * Module.t option) list
    -> t

  type classified =
    { tight : Module.t list Lib.Map.t
      (** Per-pair tight entries: libs mapped to the [Some]-entry modules
          referenced. Candidates for [deps_of_entry_modules] unless the lib also
          appears in [non_tight] (mixed-entry libs must glob to cover their
          [None] entries). *)
    ; non_tight : Lib.Set.t
      (** Libs whose [None]-entry modules appear in the input (wrapped locals,
          externals, or unwrapped locals with some staged-pps /
          instrumentation-only entries). The caller must glob these. *)
    }

  (** Classify the libraries whose entry modules appear in [referenced_modules].
      A lib with mixed [Some]/[None] entries can appear in BOTH [tight] (for its
      [Some] modules) AND [non_tight] (for its [None] modules). *)
  val filter_libs_with_modules : t -> referenced_modules:Module_name.Set.t -> classified

  (** [(lib, entry module)] pairs for the cross-library walk; excludes
      [no_ocamldep] libs and entries with [m_opt = None]. *)
  val lookup_tight_entries : t -> Module_name.t -> (Lib.t * Module.t) list

  (** True for libs with at least one tight-eligible ([Some]) entry. Used to
      drop unreached libs from a consumer's compile deps: if the lib is capable
      of tight-eligibility but no module of it is referenced, the link rule
      still pulls it in. *)
  val is_tight_eligible : t -> Lib.t -> bool

  (** Local wrapped libs whose entry name is in [referenced_modules]. The
      consumer must glob the wrapped lib's [Lib.closure] (see the file-level
      comment for why). *)
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
