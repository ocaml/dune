module Ppx_args : sig
  module Cookie : sig
    type t =
      { name : string
      ; value : String_with_vars.t
      }
  end

  type t = { cookies : Cookie.t list }

  val empty : t
end

type t =
  | Normal
  | Ppx_deriver of Ppx_args.t
  | Ppx_rewriter of Ppx_args.t

val to_dyn : t Dyn.builder
val equal : t -> t -> bool

include Dune_sexp.Conv.S with type t := t
