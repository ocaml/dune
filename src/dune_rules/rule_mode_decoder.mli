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

(** [is_ignored mode ~until_clean] will return if a rule with [mode] should be
    ignored whenever [--ignored-promoted-rules] is set.

    [until_clean] is used to set if [(promote (until-clean))] is ignored as
    considered by this function. Old versions of dune would incorrectly ignore
    this, so we need to maintain the old behavior for now. *)
val is_ignored : Rule.Mode.t -> until_clean:[ `Ignore | `Keep ] -> bool
