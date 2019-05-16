type t =
  { has_native : bool
  ; ext_lib : string
  ; ext_obj : string
  ; os_type : string
  ; architecture : string
  ; system : string
  ; model : string
  }

val allowed_in_enabled_if : string list

val get_for_enabled_if : t -> var:string -> string
