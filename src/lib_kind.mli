type t =
  | Normal
  | Ppx_deriver
  | Ppx_rewriter

include Dune_lang.Conv with type t := t
