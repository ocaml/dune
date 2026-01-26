open Import

val syntax : Syntax.t

type t =
  { merge_into : string option
  ; flags : Ordered_set_lang.Unexpanded.t
  ; modules : Ordered_set_lang.Unexpanded.t
  ; mode : Rule_mode.t
  ; loc : Loc.t
  ; infer : bool
  ; enabled_if : Blang.t
  ; explain : Blang.t option
  ; menhir_syntax : Syntax.Version.t
  }

include Stanza.S with type t := t
