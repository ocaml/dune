open Import

type allowed_vars =
  | Any
  | Only of (string * Syntax.Version.t) list

val common_vars : since:Syntax.Version.t -> allowed_vars

val decode
  :  allowed_vars:allowed_vars
  -> ?is_error:bool
  -> since:Syntax.Version.t option
  -> unit
  -> Blang.t Decoder.fields_parser

val decode_value
  :  allowed_vars:allowed_vars
  -> ?is_error:bool
  -> unit
  -> Blang.t Decoder.t
