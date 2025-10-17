open Import

type t =
  { name : string
  ; args : t list
  }

val of_string : string -> t
val to_string : t -> string
val to_module_name : t -> Module_name.Unique.t
