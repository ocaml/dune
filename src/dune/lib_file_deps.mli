module Group : sig
  type t =
    | Cmi
    | Cmx
    | Header
end

val deps : Lib.L.t -> groups:Group.t list -> Dep.Set.t
(** [deps t libs ~files] returns a list of path dependencies for all the files
    with extension [files] of libraries [libs]. *)

val deps_with_exts : (Lib.t * Group.t list) list -> Dep.Set.t
