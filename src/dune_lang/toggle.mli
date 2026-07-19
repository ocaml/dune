open Import
include module type of Stdune.Toggle with type t = Stdune.Toggle.t

val encode : t Encoder.t
val decode : t Decoder.t
val field : ?check:unit Decoder.t -> string -> t option Decoder.fields_parser
