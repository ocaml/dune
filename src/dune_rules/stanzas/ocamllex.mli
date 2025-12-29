open Import

type t =
  { loc : Loc.t
  ; modules : Ordered_set_lang.Unexpanded.t
  ; mode : Rule_mode.t
  ; enabled_if : Blang.t
  }

val decode : t Dune_lang.Decoder.t
val modules_settings : t -> Modules_settings.t

include Stanza.S with type t := t
