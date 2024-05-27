(** Findlib database *)

open Import

(** Findlib database *)
type t

module Unavailable_reason : sig
  type t =
    | Not_found
    (** The package is hidden because it contains an unsatisfied 'exists_if'
        clause *)
    | Invalid_dune_package of User_message.t

  val to_dyn : t -> Dyn.t
end

(** Lookup a whole package, including sub-packages, in the given database.
    [root_name] must be a library name without dots. *)
val find_root_package
  :  t
  -> Package.Name.t
  -> (Dune_package.t, Unavailable_reason.t) result Memo.t

val find : t -> Lib_name.t -> (Dune_package.Entry.t, Unavailable_reason.t) result Memo.t

(** List all the packages available in this Database *)
val all_packages : t -> Dune_package.Entry.t list Memo.t

(** List all the packages that have broken [dune-package] files *)
val all_broken_packages : t -> (Package.Name.t * User_message.t) list Memo.t

val create : Context_name.t -> t Memo.t

module For_tests : sig
  val create : paths:Path.t list -> lib_config:Lib_config.t -> t Memo.t
end
