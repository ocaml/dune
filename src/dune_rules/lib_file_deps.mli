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
      module when it is known ([Some] for local libraries; [None] for
      externals). Libraries whose entries all carry a [Module.t] and
      which are unwrapped are eligible for per-module deps.

      [no_ocamldep] names local libs whose ocamldep output is
      short-circuited (single-module stanzas without library
      dependencies — see [Dep_rules.skip_ocamldep]). The cross-lib
      BFS in [module_compilation] must not try to read [.d] files
      for those libs. *)
  val create
    :  ?no_ocamldep:Lib.Set.t
    -> (Module_name.t * Lib.t * Module.t option) list
    -> t

  type classified =
    { unwrapped : Module.t list Lib.Map.t
      (** Directly-referenced libraries that are local, unwrapped, and
        whose referenced entry modules are all known as [Module.t].
        Each is mapped to the list of [Module.t]s the consumer
        references. These libraries' shape allows per-module deps via
        [deps_of_entry_modules]; whether to use it is the caller's
        policy. *)
    ; wrapped : Lib.t list
      (** Other directly-referenced libraries — wrapped locals, all
        externals, or anything else for which we fall back to a glob.
        Sorted by [Lib.compare]. *)
    }

  (** Classify the libraries whose entry modules appear in
      [referenced_modules]. *)
  val filter_libs_with_modules : t -> referenced_modules:Module_name.Set.t -> classified

  (** [lookup_tight_entries idx name] returns [(lib, entry module)]
      pairs used by the cross-library BFS in [module_compilation].
      Libraries in [no_ocamldep] are excluded (their [.d] files do
      not exist), as are externals, wrapped locals, and entries with
      no [Module.t]. *)
  val lookup_tight_entries : t -> Module_name.t -> (Lib.t * Module.t) list

  (** [tight_subset idx lib names] returns [lib]'s entry modules
      whose names appear in [names]. Returns [[]] when [lib] is not
      tight-eligible; callers should interpret that as a signal to
      fall back to [deps_of_entries] (a glob over the lib's objdir). *)
  val tight_subset : t -> Lib.t -> Module_name.Set.t -> Module.t list
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
