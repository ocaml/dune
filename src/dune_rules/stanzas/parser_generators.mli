open Import

type t =
  { loc : Loc.t
  ; modules : Ordered_set_lang.Unexpanded.t
  ; mode : Rule_mode.t
  ; enabled_if : Blang.t
  }

type for_ =
  | Ocamllex of t
  | Ocamlyacc of t

val since_expanded : Dune_lang.Syntax.Version.t
val tool : for_ -> string
val decode : t Dune_lang.Decoder.t

module Ocamllex : Stanza.S with type t := t
module Ocamlyacc : Stanza.S with type t := t
