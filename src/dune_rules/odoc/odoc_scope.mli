(** Naming of libraries and packages in the generated documentation *)

open Import

module Scope_key : sig
  (** [of_string context s] finds the library named by [s], which is either a
      public library name or a private library unique name. *)
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
end

(** Unique name of a local library: its name if it is public, or
    [name@key] where [key] identifies its project if it is private. *)
val lib_unique_name : Lib.t -> string

(** Package of the library if it has one, its unique name otherwise. *)
val pkg_or_lnu : Lib.t -> string
