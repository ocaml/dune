open! Stdune

module Make (S : Dune_util.Stringlike_base) = struct
  module T : Dune_util.Stringlike with type t := S.t = Dune_util.Stringlike.Make (S)

  type t = Loc.t * S.t

  let to_string (_, s) = T.to_string s
  let of_string string = Loc.none, T.of_string string

  let of_string_opt string =
    T.of_string_opt string |> Option.map ~f:(fun s -> Loc.none, s)
  ;;

  let parse_string_exn (loc, string) = loc, T.parse_string_exn (loc, string)

  let of_string_user_error (loc, string) =
    Result.map (T.of_string_user_error (loc, string)) ~f:(fun s -> loc, s)
  ;;

  let to_dyn (_, s) = T.to_dyn s
  let decode = T.decode_loc
  let encode (_, s) = T.encode s
  let decode_loc = Dune_sexp.Decoder.located decode

  let conv =
    let from, to_ = T.conv in
    let from s = Result.map (from s) ~f:(fun r -> Loc.none, r) in
    let to_ formatter (_, s) = to_ formatter s in
    from, to_
  ;;
end
