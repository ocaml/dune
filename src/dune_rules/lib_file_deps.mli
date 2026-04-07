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

module Lib_index : sig
  type entry = Lib.t * Module.t option
  type t

  val empty : t

  val create
    :  Super_context.t
    -> Lib.t list
    -> for_:Compilation_mode.t
    -> t Resolve.Memo.t

  val filter_libs : t -> referenced_modules:Module_name.Set.t -> entry list
end

(** Compute library file dependencies for the given entries and cm_kind.
    Entries with [Some module_] use per-file deps; [None] uses glob deps.
    When [opaque] is true, local libraries only depend on .cmi (not .cmx). *)
val deps_of_entries
  :  opaque:bool
  -> cm_kind:Lib_mode.Cm_kind.t
  -> Lib_index.entry list
  -> Dep.Set.t
