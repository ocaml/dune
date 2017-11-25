(** Workspaces definitions *)

open! Import

module Context : sig
  module Opam : sig
    type t =
      { name   : string
      ; switch : string
      ; root   : string option
      ; merlin : bool
      ; host   : string option
      }
  end

  type t = Default | Opam of Opam.t

  val name : t -> string
end

type t =
  { merlin_context : string option
  ; contexts       : Context.t list
  }

val load : string -> t
