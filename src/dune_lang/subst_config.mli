open Import

type t = Toggle.t

val encode : t Encoder.t
val field : (Loc.t * t) option Decoder.fields_parser
val of_config : t option -> t
