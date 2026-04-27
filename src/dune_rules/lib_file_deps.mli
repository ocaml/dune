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

  (** [unwrapped_local] is the tight-eligible set (local AND
      unwrapped); [no_ocamldep] names local libs whose [.d] files
      are short-circuited away by [Dep_rules.skip_ocamldep] and
      must therefore be excluded from the cross-library walk.
      [entries] carries [Some m] for local libs, [None] for
      externals — see [Compilation_context.build_lib_index] for the
      indexing convention used for wrapped libs' children. *)
  val create
    :  no_ocamldep:Lib.Set.t
    -> unwrapped_local:Lib.Set.t
    -> entries:(Module_name.t * Lib.t * Module.t option) list
    -> t

  type classified =
    { tight : Module.t list Lib.Map.t
      (** Tight-eligible libs the consumer references, mapped to
        the referenced entry modules. *)
    ; non_tight : Lib.t list
      (** Other referenced libs (wrapped locals, externals, …),
        sorted by [Lib.compare]. *)
    }

  (** Classify the libs whose entry modules appear in
      [referenced_modules]. *)
  val filter_libs_with_modules : t -> referenced_modules:Module_name.Set.t -> classified

  (** Like [filter_libs_with_modules] but returns only the [tight]
      map. *)
  val tight_modules_per_lib
    :  t
    -> referenced_modules:Module_name.Set.t
    -> Module.t list Lib.Map.t

  (** Walkable entries indexed under [name]: wrappers and children
      of wrapped libs alike (see [Compilation_context.build_lib_
      index] for the indexing convention). Libs in [no_ocamldep]
      and externals are excluded. *)
  val lookup_walkable_entries : t -> Module_name.t -> (Lib.t * Module.t) list

  (** A library is tight-eligible when local and unwrapped. The
      walk has full visibility into such libs, so absence of all
      their entry modules from the post-walk reference set means
      the consumer doesn't reach the lib; the compile rule needs
      no dep on it. *)
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
