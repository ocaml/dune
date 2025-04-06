open Dune_util.Synopsis

let decode =
  let open Dune_sexp.Decoder in
  plain_string (fun ~loc:_ s -> of_string s)
;;
