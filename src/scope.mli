(** Scopes *)

(** Representation of a Scope. It contain a library database for all
    the private libraries in the scope. *)
type t

val root : t -> Path.t
val name : t -> string option
val info : t -> Jbuild.Scope_info.t

(** Return the library database associated to this scope *)
val libs : t -> Lib.DB.t

(** Scope databases *)
module DB : sig
  type scope = t

  type t

  (** Return the new scope database as well as the public libraries
      database *)
  val create
    :  scopes:Jbuild.Scope_info.t list
    -> context:string
    -> installed_libs:Lib.DB.t
    -> (Path.t * Jbuild.Library.t) list
    -> t * Lib.DB.t

  val find_by_dir  : t -> Path.t        -> scope
  val find_by_name : t -> string option -> scope
end with type scope := t
