open Import

type allowed_vars =
  | Any
  | Only of (string * Dune_lang.Syntax.Version.t) list

val common_vars : since:Dune_lang.Syntax.Version.t -> allowed_vars

val decode
  :  allowed_vars:allowed_vars
  -> ?is_error:bool
  -> since:Dune_lang.Syntax.Version.t option
  -> unit
  -> Blang.t Dune_lang.Decoder.fields_parser

val decode_value
  :  allowed_vars:allowed_vars
  -> ?is_error:bool
  -> unit
  -> Blang.t Dune_lang.Decoder.t
