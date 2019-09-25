(** Scopes *)

(** A scope is a project + a library database *)

open! Stdune

type t

val root : t -> Path.Build.t

val name : t -> Dune_project.Name.t

val project : t -> Dune_project.t

(** Return the library database associated to this scope *)
val libs : t -> Lib.DB.t

(** Scope databases *)
module DB : sig
  type scope = t

  type t

  (** Return the new scope database as well as the public libraries database *)
  val create :
       projects:Dune_project.t list
    -> context:string
    -> installed_libs:Lib.DB.t
    -> lib_config:Lib_config.t
    -> Lib.DB.Library_related_stanza.t list
    -> t * Lib.DB.t

  val find_by_dir : t -> Path.Build.t -> scope

  val find_by_project : t -> Dune_project.t -> scope
end
with type scope := t
