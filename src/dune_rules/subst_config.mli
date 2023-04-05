type t =
  | Disabled
  | Enabled

val is_enabled : t -> bool

val to_dyn : t -> Dyn.t

val encode : t Dune_lang.Encoder.t

val field :
  since:Dune_lang.Syntax.Version.t -> t option Dune_lang.Decoder.fields_parser

val of_config : t option -> t
