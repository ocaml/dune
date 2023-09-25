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

val deps_with_exts : (Lib.t * Group.t list) list -> Dep.Set.t

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
