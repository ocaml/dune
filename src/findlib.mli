(** Findlib database *)

open Stdune

(** Findlib database *)
type t

val create
  :  stdlib_dir:Path.t
  -> path:Path.t list
  -> t

(** The search path for this DB *)
val path : t -> Path.t list

(** [root_package_name "foo.*"] is "foo" *)
val root_package_name : string -> string

module Package : sig
  (** Representation of a findlib package *)
  type t

  val meta_file        : t -> Path.t
  val name             : t -> string
  val dir              : t -> Path.t
  val version          : t -> string option
  val description      : t -> string option
  val archives         : t -> Path.t list Mode.Dict.t
  val plugins          : t -> Path.t list Mode.Dict.t
  val jsoo_runtime     : t -> Path.t list
  val requires         : t -> string list
  val ppx_runtime_deps : t -> string list
  val dune_file        : t -> Path.t option
end

module Unavailable_reason : sig
  type t =
    | Not_found
    (** The package is hidden because it contains an unsatisfied
        'exist_if' clause *)
    | Hidden of Package.t

  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

(** Lookup a package in the given database *)
val find : t -> string -> (Package.t, Unavailable_reason.t) result

val available : t -> string -> bool

(** List all the packages available in this Database *)
val all_packages  : t -> Package.t list

(** List all the packages that are not available in this database *)
val all_unavailable_packages : t -> (string * Unavailable_reason.t) list

module Config : sig
  type t
  val load : Path.t -> toolchain:string -> context:string -> t
  val get : t -> string -> string option
end
