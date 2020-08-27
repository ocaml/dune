(** Scopes *)
open! Dune_engine

(** A scope is a project + a library database *)

open! Stdune

type t

val root : t -> Path.Build.t

val project : t -> Dune_project.t

(** Return the library database associated to this scope *)
val libs : t -> Lib.DB.t

val coq_libs : t -> Coq_lib.DB.t

(** Scope databases *)
module DB : sig
  type scope = t

  type t

  (** Return the new scope database as well as the public libraries database *)
  val create_from_stanzas :
       projects:Dune_project.t list
    -> context:Context.t
    -> installed_libs:Lib.DB.t
    -> Dune_load.Dune_file.t list
    -> t * Lib.DB.t

  val find_by_dir : t -> Path.Build.t -> scope

  val find_by_project : t -> Dune_project.t -> scope
end
with type scope := t
