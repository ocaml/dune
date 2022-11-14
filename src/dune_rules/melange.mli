open! Import

val syntax : Dune_lang.Syntax.t

val extension_key : unit Dune_engine.Dune_project.Extension.t

val js_ext : string

module Module_system : sig
  type t =
    | Es6
    | CommonJs

  val to_string : t -> string
end

module Cm_kind : sig
  type t =
    | Cmi
    | Cmj

  val source : t -> Ocaml.Ml_kind.t

  val ext : t -> string

  val to_dyn : t -> Dyn.t

  module Map : sig
    type 'a t =
      { cmi : 'a
      ; cmj : 'a
      }

    val make_all : 'a -> 'a t
  end
end

val js_basename : Module.t -> Filename.t
