open Import

module Ppx_args = struct
  module Cookie = struct
    type t =
      { name : string
      ; value : String_with_vars.t
      }

    let to_dyn x =
      let open Dyn in
      record [ "name", string x.name; "value", String_with_vars.to_dyn x.value ]
    ;;

    let decode =
      let open Decoder in
      let* () = Syntax.since Stanza.syntax (1, 10) in
      enter
        (let+ name =
           plain_string (fun ~loc str ->
             if String.contains str '='
             then
               User_error.raise
                 ~loc
                 [ Pp.text "Character '=' is not allowed in cookie names" ]
             else str)
         and+ value = String_with_vars.decode in
         { name; value })
    ;;

    let encode { name; value } =
      let open Dune_sexp in
      List [ Encoder.string name; String_with_vars.encode value ]
    ;;
  end

  type t = { cookies : Cookie.t list }

  let empty = { cookies = [] }

  let to_dyn { cookies } =
    let open Dyn in
    record [ "cookies", list Cookie.to_dyn cookies ]
  ;;

  let decode =
    let open Decoder in
    let args =
      let+ cookies = field "cookies" (repeat Cookie.decode) ~default:[] in
      { cookies }
    in
    fields args
  ;;

  let encode { cookies } =
    let open Encoder in
    record_fields [ field_l "cookies" Cookie.encode cookies ]
  ;;
end

module Dune_file = struct
  type t =
    | Normal
    | Ppx_deriver of Ppx_args.t
    | Ppx_rewriter of Ppx_args.t

  let cstr_name = function
    | Normal -> "normal"
    | Ppx_deriver _ -> "ppx_deriver"
    | Ppx_rewriter _ -> "ppx_rewriter"
  ;;

  let decode =
    let open Decoder in
    sum
      [ "normal", return Normal
      ; ( "ppx_deriver"
        , let+ args = Ppx_args.decode in
          Ppx_deriver args )
      ; ( "ppx_rewriter"
        , let+ args = Ppx_args.decode in
          Ppx_rewriter args )
      ]
  ;;

  let encode t =
    match
      match t with
      | Normal -> Dune_sexp.atom (cstr_name t)
      | Ppx_deriver x | Ppx_rewriter x ->
        List (Dune_sexp.atom (cstr_name t) :: Ppx_args.encode x)
    with
    | List [ x ] -> x
    | x -> x
  ;;

  let to_dyn x =
    let open Dyn in
    match x with
    | Normal -> variant "Normal" []
    | Ppx_deriver args -> variant "Ppx_deriver" [ Ppx_args.to_dyn args ]
    | Ppx_rewriter args -> variant "Ppx_rewriter" [ Ppx_args.to_dyn args ]
  ;;
end

type t =
  | Virtual
  | Parameter
  | Dune_file of Dune_file.t

let to_dyn x =
  let open Dyn in
  match x with
  | Virtual -> variant "Virtual" []
  | Parameter -> variant "Parameter" []
  | Dune_file t -> variant "Dune_file" [ Dune_file.to_dyn t ]
;;

let decode =
  let open Decoder in
  (* TODO: Less code reuse with either? *)
  map ~f:(fun k -> Dune_file k) Dune_file.decode
  <|> enum [ "parameter", Parameter; "virtual", Virtual ]
;;

let encode t =
  match
    match t with
    | Virtual -> Dune_sexp.atom "virtual"
    | Parameter -> Dune_sexp.atom "parameter"
    | Dune_file d -> Dune_file.encode d
  with
  | List [ x ] -> x
  | x -> x
;;
