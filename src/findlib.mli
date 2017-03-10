(** Findlib database *)

exception Package_not_found of string

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
  ; archives         : string list Mode.Dict.t
  ; plugins          : string list Mode.Dict.t
  ; requires         : package list
  ; ppx_runtime_deps : package list
  ; has_headers      : bool
  }

val find     : t -> string -> package option
val find_exn : t -> string -> package

val available : t -> string -> bool

val root_package_name : string -> string

val closure : package list -> package list
val closed_ppx_runtime_deps_of : package list -> package list

val root_packages : t -> string list
val all_packages  : t -> package list
