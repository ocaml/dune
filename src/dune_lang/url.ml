open Import

module T = struct
  type nonrec t = OpamUrl.t

  let to_dyn t = Dyn.string (OpamUrl.to_string t)
  let compare x y = Ordering.of_int (OpamUrl.compare x y)
end

include T

let decode_loc =
  let open Dune_sexp.Decoder in
  map_validate (located string) ~f:(fun (loc, s) ->
    match OpamUrl.of_string s with
    | url -> Ok (loc, url)
    | exception OpamUrl.Parse_error m ->
      Error (User_message.make [ Pp.text "invalid url "; Pp.text m ]))
;;

let to_string = OpamUrl.to_string
