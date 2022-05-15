(** Scopes *)

(** A scope is a project + a library database *)

open Import

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
    -> projects_by_package:Dune_project.t Package.Name.Map.t
    -> context:Context.t
    -> installed_libs:Lib.DB.t
    -> modules_of_lib:
         (dir:Path.Build.t -> name:Lib_name.t -> Modules.t Memo.t) Fdecl.t
    -> Dune_file.t list
    -> (t * Lib.DB.t) Memo.t

  val find_by_dir : t -> Path.Build.t -> scope

  val find_by_project : t -> Dune_project.t -> scope
end
with type scope := t
