open Import

module Include : sig
  type t = Loc.t * string

  include Stanza.S with type t := t
end

module Dynamic_include : sig
  type t = Include.t

  include Stanza.S with type t := t
end

module Enabled_if : sig
  type t =
    { condition : Blang.t
    ; body : Dune_lang.Ast.t list
    }

  include Stanza.S with type t := t
end

val stanza_package : Stanza.t -> Package.Id.t option
