(** Findlib database *)

exception Package_not_found of string

(** Findlib database *)
type t

val create : Context.t -> t

val context : t -> Context.t

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

val find_exn : t -> string -> package

val available : t -> string -> bool

val root_package_name : string -> string

val closure : package list -> package list
val closed_ppx_runtime_deps_of : package list -> package list

val root_packages : t -> string list
val all_packages  : t -> package list
