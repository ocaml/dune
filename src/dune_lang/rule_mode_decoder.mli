open Import

module Promote : sig
  val decode : Rule_mode.Promote.t Decoder.t
  val into_decode : Rule_mode.Promote.Into.t Decoder.t
end

val decode : Rule_mode.t Decoder.t
val field : Rule_mode.t Decoder.fields_parser
