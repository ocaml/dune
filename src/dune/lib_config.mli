open Stdune

type t =
  { has_native : bool
  ; ext_lib : string
  ; ext_obj : string
  ; os_type : string
  ; architecture : string
  ; system : string
  ; model : string  (** Native dynlink *)
  ; natdynlink_supported : Dynlink_supported.By_the_os.t
  ; ext_dll : string
  ; stdlib_dir : Path.t
  }

val allowed_in_enabled_if : string list

val get_for_enabled_if : t -> var:string -> string
