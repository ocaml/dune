open Dune_util.Synopsis
open Dune_sexp

let decode =
  let open Decoder in
  plain_string (fun ~loc:_ s -> of_string s)
;;

let encode t =
  let open Encoder in
  t |> value |> string
;;
