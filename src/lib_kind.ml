module Ppx_args = struct
  module Cookie = struct
    type t =
      { name : string
      ; value : String_with_vars.t
      }

    let decode =
      let open Stanza.Decoder in
      let+ name = string
      and+ value = String_with_vars.decode in
      return { name; value }

    let encode { name; value } =
      let open Dune_lang.Encoder in
      List
        [ string name
        ; String_with_vars.encode value
        ]
  end

  type t =
    { cookies : Cookies.t list
    }

  let decode =
    let open Stanza.Decoder in
    fields
      (let+ cookies = field "cookies" (list Cookie.decode) ~default:[] in
       { cookies })

  let encode { cookies } =
    let open Dune_lang.Encoder in
    record_fields
      [ field_l "cookies" Cookies.encode cookies ]
end

type t =
  | Normal
  | Ppx_deriver of Ppx_args.t
  | Ppx_rewriter of Ppx_args.t

let decode =
  let open Dune_lang.Decoder in
  enum
    [ "normal"       , Normal
    ; "ppx_deriver"  , Ppx_deriver
    ; "ppx_rewriter" , Ppx_rewriter
    ]

let encode t =
  let open Dune_lang.Encoder in
  match
    match t with
    | Normal -> Dune_lang.atom "normal"
    | Ppx_deriver x ->
      List (Dune_lang.atom "ppx_deriver" :: Ppx_args.encode x)
    | Ppx_rewriter x ->
      List (Dune_lang.atom "ppx_rewriter" :: Ppx_args.encode x)
  with
  | List [x] -> x
  | x -> x
