open Import

module Group : sig
  type ocaml =
    | Cmi
    | Cmx

  type melange =
    | Cmi
    | Cmj
    | Js

  type t =
    | Ocaml of ocaml
    | Melange of melange
    | Header

  val to_predicate : t -> string Predicate_with_id.t
end

(** [deps t libs ~files] returns a list of path dependencies for all the files
    with extension [files] of libraries [libs]. *)
val deps : Lib.t list -> groups:Group.t list -> Dep.Set.t

val deps_with_exts : (Lib.t * Group.t list) list -> Dep.Set.t
