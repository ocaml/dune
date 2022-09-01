open! Import

module Promote : sig
  val decode : Rule.Promote.t Dune_lang.Decoder.t

  val into_decode : Rule.Promote.Into.t Dune_lang.Decoder.t
end

module Extended : sig
  type t =
    | Normal of Rule.Mode.t
    | Patch_back_source_tree

  val field : t Dune_lang.Decoder.fields_parser
end

val decode : Rule.Mode.t Dune_lang.Decoder.t

val field : Rule.Mode.t Dune_lang.Decoder.fields_parser
