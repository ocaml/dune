open! Import

module Promote : sig
  val decode : Rule.Promote.t Dune_lang.Decoder.t
  val into_decode : Rule.Promote.Into.t Dune_lang.Decoder.t
end

val decode : Rule.Mode.t Dune_lang.Decoder.t
val field : Rule.Mode.t Dune_lang.Decoder.fields_parser
