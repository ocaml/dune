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
  val find_by_dir : Path.Build.t -> t Memo.t

  val find_by_project : Context.t -> Dune_project.t -> t Memo.t

  val public_libs : Context.t -> Lib.DB.t Memo.t

  module Lib_entry : sig
    type t =
      | Library of Lib.Local.t
      | Deprecated_library_name of Dune_file.Deprecated_library_name.t
  end

  val lib_entries_of_package :
    Context.t -> Package.Name.t -> Lib_entry.t list Memo.t

  val with_all : Context.t -> f:((Dune_project.t -> t) -> 'a) -> 'a Memo.t
end
