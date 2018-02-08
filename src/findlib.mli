(** Findlib database *)

open Import

module Package_not_available : sig
  type t =
    { package     : string
    ; required_by : With_required_by.Entry.t list
    ; reason      : reason
    }

  and reason =
    | Not_found
    | Hidden
    (** exist_if not satisfied *)
    | Dependencies_unavailable of t list
    (** At least one dependency is unavailable *)

  val top_closure : t list -> t list

  (** Explain why a package is not available *)
  val explain : Format.formatter -> reason -> unit
end

module External_dep_conflicts_with_local_lib : sig
  type t =
    { package             : string
    ; required_by         : With_required_by.Entry.t
    ; required_locally_in : With_required_by.Entry.t list
    ; defined_locally_in  : Path.t
    }
end

type error =
  | Package_not_available of Package_not_available.t
  | External_dep_conflicts_with_local_lib of External_dep_conflicts_with_local_lib.t

exception Findlib of error

(** Findlib database *)
type t

val create
  :  stdlib_dir:Path.t
  -> path:Path.t list
  -> t

val path : t -> Path.t list

module Package : sig
  (** Representation of a findlib package *)
  type t

  val name        : t -> string
  val dir         : t -> Path.t
  val version     : t -> string
  val description : t -> string

  (** Package files *)
  val archives     : t -> Mode.t -> Path.t list
  val plugins      : t -> Mode.t -> Path.t list
  val jsoo_runtime : t -> string list

  val requires         : t -> t list
  val ppx_runtime_deps : t -> t list
end

val find
  :  t
  -> required_by:With_required_by.Entry.t list
  -> string
  -> Package.t option
val find_exn
  :  t
  -> required_by:With_required_by.Entry.t list
  -> string
  -> Package.t

(** Same as [Option.is_some (find t ...)] *)
val available : t -> required_by:With_required_by.Entry.t list -> string -> bool

(** [root_package_name "foo.*"] is "foo" *)
val root_package_name : string -> string

(** [local_public_libs] is a map from public library names to where they are defined in
    the workspace. These must not appear as dependency of a findlib package *)
val closure
  :  Package.t list
  -> required_by:With_required_by.Entry.t list
  -> local_public_libs:Path.t String_map.t
  -> Package.t list
val closed_ppx_runtime_deps_of
  :  Package.t list
  -> required_by:With_required_by.Entry.t list
  -> local_public_libs:Path.t String_map.t
  -> Package.t list

val root_packages : t -> string list
val all_packages  : t -> Package.t list
val all_unavailable_packages : t -> Package_not_available.t list

val stdlib_with_archives : t -> Package.t

module Config : sig
  type t
  val load : Path.t -> toolchain:string -> context:string -> t
  val get : t -> string -> string
end
