type t =
  | Disabled
  | Enabled

val is_enabled : t -> bool
val to_dyn : t -> Dyn.t
val encode : t Dune_sexp.Encoder.t
val field : since:Dune_sexp.Syntax.Version.t -> t option Dune_sexp.Decoder.fields_parser
val of_config : t option -> t
