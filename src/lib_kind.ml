type t =
  | Normal
  | Ppx_deriver
  | Ppx_rewriter

let decode =
  let open Dune_lang.Decoder in
  enum
    [ "normal"       , Normal
    ; "ppx_deriver"  , Ppx_deriver
    ; "ppx_rewriter" , Ppx_rewriter
    ]

let encode t =
  Dune_lang.Encoder.string (
    match t with
    | Normal -> "normal"
    | Ppx_deriver -> "ppx_deriver"
    | Ppx_rewriter -> "ppx_rewriter")
