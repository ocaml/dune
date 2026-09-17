open Import

type qualification =
  | Unqualified
  | Qualified of { dirs : File_binding.Unexpanded.t list }

type t =
  | No
  | Include of qualification

type stanza = Loc.t * t

include Stanza.S with type t := stanza

val decode : qualified:unit Decoder.t -> (Loc.t * t) Decoder.t
