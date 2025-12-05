open Import

module Module_system : sig
  type t =
    | ESM
    | CommonJS

  val default : t * Filename.Extension.t
  val to_string : t -> string
end

module Cm_kind : module type of Dune_lang.Melange.Cm_kind

module Install : sig
  val dir : string
end

val js_basename : Module.t -> Filename.t
