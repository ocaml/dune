open! Import

type lock = Lock of String_with_vars.t

type t = lock list

let decode_one =
  let open Dune_lang.Decoder in
  let+ sw = Dune_engine.String_with_vars.decode in
  Lock sw

let field ?(check = Dune_lang.Decoder.return ()) () =
  let open Dune_lang.Decoder in
  field "locks" (check >>> repeat decode_one) ~default:[]
