(** Workspaces definitions *)

open! Import

module Context : sig
  module Opam : sig
    type t =
      { name   : string
      ; switch : string
      ; root   : string option
      }
  end

  type t = Default | Opam of Opam.t
end

type t = Context.t list

val load : string -> t
