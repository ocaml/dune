(** Findlib database *)

open! Stdune
open Import

(** Findlib database *)
type t

val meta_fn : string

val create :
  stdlib_dir:Path.t -> paths:Path.t list -> version:Ocaml_version.t -> t

(** The search path for this DB *)
val paths : t -> Path.t list

module Package : sig
  val preds : Variant.Set.t
end

module Unavailable_reason : sig
  type t =
    | Not_found
        (** The package is hidden because it contains an unsatisfied 'exist_if'
            clause *)
    | Hidden of Dune_package.Lib.t
    | Invalid_dune_package of exn

  val to_string : t -> string

  val to_dyn : t -> Dyn.t
end

(** Lookup a package in the given database *)
val find :
  t -> Lib_name.t -> (Dune_package.Entry.t, Unavailable_reason.t) result

val available : t -> Lib_name.t -> bool

(** List all the packages available in this Database *)
val all_packages : t -> Dune_package.Entry.t list

(** List all the packages that are not available in this database *)
val all_unavailable_packages : t -> (Lib_name.t * Unavailable_reason.t) list

(** A dummy package. This is used to implement [external-lib-deps] *)
val dummy_package : t -> name:Lib_name.t -> Dune_package.Lib.t

module Config : sig
  type t

  val to_dyn : t -> Dyn.t

  val load : Path.t -> toolchain:string -> context:string -> t

  val get : t -> string -> string option

  val env : t -> Env.t
end
