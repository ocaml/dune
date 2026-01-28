(** Scopes *)

(** A scope is a project + a library database *)

open Import

type t

val root : t -> Path.Build.t
val project : t -> Dune_project.t

(** Return the library database associated to this scope *)
val libs : t -> Lib.DB.t

(** Return the Rocq library database associated to this scope *)
val coq_libs : t -> Coq_lib.DB.t Memo.t

val rocq_libs : t -> Rocq_lib.DB.t Memo.t

module DB : sig
  val packages : unit -> Package.t Package.Name.Map.t Memo.t
  val mask : unit -> Only_packages.t Memo.t
  val find_by_dir : Path.Build.t -> t Memo.t
  val find_by_project : Context_name.t -> Dune_project.t -> t Memo.t
  val public_libs : Context_name.t -> Lib.DB.t Memo.t

  module Lib_entry : sig
    type t =
      | Library of Lib.Local.t
      | Deprecated_library_name of Deprecated_library_name.t

    module Set : sig
      type entry := t

      type t =
        { libraries : Lib.Local.t list
        ; deprecated_library_names : Deprecated_library_name.t list
        }

      val partition_map : t -> f:(entry -> ('a, 'b) Either.t) -> 'a list * 'b list
    end
  end

  val lib_entries_of_package : Context_name.t -> Package.Name.t -> Lib_entry.Set.t Memo.t
  val with_all : Context.t -> f:((Dune_project.t -> t) -> 'a) -> 'a Memo.t
end
