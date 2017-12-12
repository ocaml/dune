(** Workspaces definitions *)

open! Import

module Context : sig
  module Target : sig
    type t =
      | Native
      | Named of string
  end
  module Opam : sig
    type t =
      { name   : string
      ; switch : string
      ; root   : string option
      ; merlin : bool
      ; host   : string option
      ; targets : Target.t list
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
