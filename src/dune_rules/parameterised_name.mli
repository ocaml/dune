open Import

type 'a t =
  { name : 'a
  ; args : 'a t list
  }

val of_string : string -> Lib_name.t t
val to_string : Lib_name.t t -> string
val to_module_name : Module_name.t t -> Module_name.Unique.t

module Scope : sig
  type t = Path.Source.t option

  val encode : t -> string
  val decode : string -> t
end
