open Import

module Name : sig
  type t

  include Stringlike with type t := t

  val equal : t -> t -> bool
  val hash : t -> int
  val repr : t Repr.t

  module Map : Map.S with type key = t
end

val check_enabled : unit Dune_lang.Decoder.t

module Definition : sig
  type t =
    { loc : Loc.t
    ; name : Name.t
    ; executable : string
    }

  include Stanza.S with type t := t

  val decode : t Dune_lang.Decoder.t
end

module Invocation : sig
  type t =
    { loc : Loc.t
    ; name : Name.t
    ; arguments : Dune_lang.Ast.t list
    }

  include Stanza.S with type t := t

  val of_ast : Dune_lang.Ast.t -> t option
end
