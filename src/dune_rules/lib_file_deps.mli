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

(** [deps_of_entry_modules ~opaque ~cm_kind lib modules] computes the
    file dependencies on the specific [modules] of [lib], using
    [Obj_dir.Module.cm_file_exn] for path construction. Only valid for
    local libraries (for which [Module.t] is available). *)
val deps_of_entry_modules
  :  opaque:bool
  -> cm_kind:Lib_mode.Cm_kind.t
  -> Lib.t
  -> Module.t list
  -> Dep.Set.t

module Lib_index : sig
  type t

  val empty : t

  (** Create an index. Each entry carries the [Module.t] of the entry
      module when it is known ([Some] for local libraries, wrapped
      or not; [None] for externals).

      [unwrapped_local] is the set of libs that are local and
      unwrapped — the tight-eligible class. The cross-library walk
      reads ocamldep for any local lib's entries (via
      [lookup_walkable_entries], including wrapped libs' children
      placed by [Compilation_context.build_lib_index] under the
      wrapper's name for auto-generated wrappers and under the
      child's own name for hand-written ones); per-module dep
      classification is restricted to [tight_eligible] (see
      [is_tight_eligible]).

      [no_ocamldep] names local libs whose ocamldep output is
      short-circuited (single-module stanzas without library
      dependencies — see [Dep_rules.skip_ocamldep]). The cross-
      library walk in [module_compilation] must not try to read
      [.d] files for those libs. *)
  val create
    :  no_ocamldep:Lib.Set.t
    -> unwrapped_local:Lib.Set.t
    -> entries:(Module_name.t * Lib.t * Module.t option) list
    -> t

  type classified =
    { tight : Module.t list Lib.Map.t
      (** Directly-referenced tight-eligible libraries (local,
        unwrapped). Mapped to the referenced entry modules. These
        libraries are candidates for per-module deps via
        [deps_of_entry_modules]; whether to emit them is the
        caller's policy. *)
    ; non_tight : Lib.t list
      (** Other directly-referenced libraries — wrapped locals,
        externals, or anything else that falls back to a glob. Sorted
        by [Lib.compare]. *)
    }

  (** Classify the libraries whose entry modules appear in
      [referenced_modules]. *)
  val filter_libs_with_modules : t -> referenced_modules:Module_name.Set.t -> classified

  (** [tight_modules_per_lib idx ~referenced_modules] builds a map
      from each tight-eligible library that exposes a name in
      [referenced_modules] to the subset of its entry modules that
      appear. Equivalent to [filter_libs_with_modules] with only the
      tight part kept. *)
  val tight_modules_per_lib
    :  t
    -> referenced_modules:Module_name.Set.t
    -> Module.t list Lib.Map.t

  (** [lookup_walkable_entries idx name] returns [(lib, entry module)]
      pairs used by the cross-library walk in [module_compilation].
      Includes wrapped local libs' wrappers and their children
      (placed in the index by [Compilation_context.build_lib_index]
      under the wrapper's name for auto-generated wrappers and
      under the child's own name for hand-written ones) — the BFS
      reads each walkable entry's ocamldep to follow alias chains
      across library boundaries. Libraries in [no_ocamldep] are
      excluded (their [.d] files do not exist); externals are
      excluded because no [Module.t] is available. *)
  val lookup_walkable_entries : t -> Module_name.t -> (Lib.t * Module.t) list

  (** [is_tight_eligible idx lib] is [true] when [lib] is local and
      unwrapped. The cross-library walk has full visibility into
      such libraries: the absence of any of their entry modules from
      the post-walk reference set is positive evidence that the
      consumer does not reach the library, so the consumer's compile
      rule does not need a dep on it. *)
  val is_tight_eligible : t -> Lib.t -> bool
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
