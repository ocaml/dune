open! Import
module Rule := Dune_engine.Rule

module Promote : sig
  val decode : Rule.Promote.t Decoder.t
  val into_decode : Rule.Promote.Into.t Decoder.t
end

val decode : Rule.Mode.t Decoder.t
val field : Rule.Mode.t Decoder.fields_parser
