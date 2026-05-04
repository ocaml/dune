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

(** [deps_of_entries ~opaque ~cm_kind entries] computes the file dependencies
    for the given library entries. When the module in an entry is [None], glob
    deps are used for the library. When [Some m], per-file deps on specific
    cm files are used. Currently all callers pass [None]; the [Some] path is
    retained for potential future per-module filtering of unwrapped libraries. *)
val deps_of_entries
  :  opaque:bool
  -> cm_kind:Lib_mode.Cm_kind.t
  -> (Lib.t * Module.t option) list
  -> Dep.Set.t

module Lib_index : sig
  type entry = Lib.t * Module.t option
  type t

  val empty : t

  (** Create an index from a list of (module_name, entry) pairs. *)
  val create : (Module_name.t * entry) list -> t

  (** Return the library entries whose module names appear in
      [referenced_modules]. *)
  val filter_libs : t -> referenced_modules:Module_name.Set.t -> entry list
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
