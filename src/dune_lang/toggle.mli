open Dune_config
module Decoder := Dune_sexp.Decoder
include module type of Config.Toggle with type t = Config.Toggle.t

val enabled : t -> bool
val of_bool : bool -> t
val encode : t Dune_sexp.Encoder.t
val decode : t Decoder.t
val field : ?check:unit Decoder.t -> string -> t option Decoder.fields_parser
