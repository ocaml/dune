(** Findlib database *)

exception Package_not_found of string

(** Findlib database *)
type t

val create : Context.t -> t

val context : t -> Context.t

val root_packages : t -> string list
val all_packages  : t -> string list

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

val find : t -> string -> package

val root_package_name : string -> string

val closure : t -> string list -> package list
val closed_ppx_runtime_deps_of : t -> string list -> package list
