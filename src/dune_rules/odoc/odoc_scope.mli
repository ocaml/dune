(** Naming of libraries and packages in the generated documentation *)

open Import

module Scope_key : sig
  (** [find_private_lib context s] is the private library without a package
      whose unique name is [s], or [None] if [s] is not such a name. *)
  val find_private_lib : Context_name.t -> string -> Lib.t option Memo.t

  (** [of_string context s] finds the library named by [s], which is either a
      public library name or a private library unique name. *)
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
end

(** Unique name of a local library: its name if it is public, or
    [name@key] where [key] identifies its project if it is private. *)
val lib_unique_name : Lib.t -> string

(** Package of the library if it has one, its unique name otherwise. *)
val pkg_or_lnu : Lib.t -> string

(** {1 Parent ID}

    See odoc's [driver.mld]. *)

module Parent_id : sig
  type t

  val to_string : t -> string
end

(** Parent id of the pages of a package: [pkg]. *)
val pkg_parent_id : Package.Name.t -> Parent_id.t

(** Parent id of the modules of a library:
    - [pkg/pkg.foo] for a public or installed library [pkg.foo];
    - [pkg/pkg.__private__.foo] for a private library [foo] of package [pkg],
      which is the name it is installed under;
    - [foo@key] for a private library [foo] without a package, where [key]
      identifies its project. *)
val lib_parent_id : Lib.t -> Parent_id.t
