open Dune_config
open Import
include module type of Config.Toggle with type t = Config.Toggle.t

val enabled : t -> bool
val of_bool : bool -> t
val encode : t Encoder.t
val decode : t Decoder.t
val field : ?check:unit Decoder.t -> string -> t option Decoder.fields_parser
