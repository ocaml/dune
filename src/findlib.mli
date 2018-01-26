(** Findlib database *)

open Import

module Package_not_available : sig
  type t =
    { package     : string
    ; required_by : string list
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
    ; required_by         : string
    ; required_locally_in : string list
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

type package =
  { name             : string
  ; dir              : Path.t
  ; version          : string
  ; description      : string
  ; archives         : Path.t list Mode.Dict.t
  ; plugins          : Path.t list Mode.Dict.t
  ; jsoo_runtime     : string list
  ; requires         : package list
  ; ppx_runtime_deps : package list
  }

val find     : t -> required_by:string list -> string -> package option
val find_exn : t -> required_by:string list -> string -> package

val available : t -> required_by:string list -> string -> bool

val root_package_name : string -> string

(** [local_public_libs] is a map from public library names to where they are defined in
    the workspace. These must not appear as dependency of a findlib package *)
val closure
  :  required_by:string list
  -> local_public_libs:Path.t String_map.t
  -> package list
  -> package list
val closed_ppx_runtime_deps_of
  :  required_by:string list
  -> local_public_libs:Path.t String_map.t
  -> package list
  -> package list

val root_packages : t -> string list
val all_packages  : t -> package list
val all_unavailable_packages : t -> Package_not_available.t list

val stdlib_with_archives : t -> package

module Config : sig
  type t
  val load : Path.t -> toolchain:string -> context:string -> t
  val get : t -> string -> string
end
