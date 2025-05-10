open Import

type lock = Lock of String_with_vars.t
type t = lock list

let decode_one =
  let open Decoder in
  let+ sw = String_with_vars.decode in
  Lock sw
;;

let field ?(check = Decoder.return ()) () =
  let open Decoder in
  field "locks" (check >>> repeat decode_one) ~default:[]
;;
