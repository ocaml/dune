type lock = Lock of String_with_vars.t
type t = lock list

let decode_one =
  let open Dune_sexp.Decoder in
  let+ sw = String_with_vars.decode in
  Lock sw
;;

let field ?(check = Dune_sexp.Decoder.return ()) () =
  let open Dune_sexp.Decoder in
  field "locks" (check >>> repeat decode_one) ~default:[]
;;
