open Import

val encode_for_rules : Action.For_shell.t Dune_lang.Encoder.t

module Replay : sig
  val encode : Action.For_shell.t Dune_lang.Encoder.t
  val decode : Action.For_shell.t Dune_lang.Decoder.t
end
