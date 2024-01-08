include OpamUrl
open Stdune

let decode_loc =
  let open Dune_sexp.Decoder in
  map_validate (located string) ~f:(fun (loc, s) ->
    match OpamUrl.of_string s with
    | url -> Ok (loc, url)
    | exception OpamUrl.Parse_error m ->
      Error (User_message.make [ Pp.text "invalid url "; Pp.text m ]))
;;

let rev t = t.hash
let hash = Poly.hash

let is_version_control t =
  match t.backend with
  | #version_control -> true
  | _ -> false
;;
