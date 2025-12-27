open Import

type t =
  { loc : Loc.t (* ; modules : Ordered_set_lang.Unexpanded.t *)
  ; modules : string list
  ; mode : Rule_mode.t
  ; enabled_if : Blang.t
  }

val decode : t Dune_lang.Decoder.t

include Stanza.S with type t := t
