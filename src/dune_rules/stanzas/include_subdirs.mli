open Import

type qualification =
  | Unqualified
  | Qualified

type t =
  | No
  | Include of qualification

type stanza = Loc.t * t

include Stanza.S with type t := stanza

val decode : enable_qualified:bool -> t Dune_lang.Decoder.t
