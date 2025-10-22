open Import

type t =
  { root_module : (Loc.t * Module_name.t) option
  ; modules_without_implementation : Ordered_set_lang.Unexpanded.t
  ; modules : Ordered_set_lang.Unexpanded.t
  }

val since_expanded : Syntax.Version.t
val decode : t Decoder.fields_parser
