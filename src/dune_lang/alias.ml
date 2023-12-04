open Stdune
open Dune_util.Alias_name

let invalid_alias = Pp.textf "%S is not a valid alias name"

let decode =
  let parse_string_exn ~syntax (loc, s) =
    let of_string_opt = if syntax >= (2, 0) then of_string_opt else of_string_opt_loose in
    match of_string_opt s with
    | None -> User_error.raise ~loc [ invalid_alias s ]
    | Some s -> s
  in
  let open Dune_sexp.Decoder in
  let* syntax = Dune_sexp.Syntax.get_exn Stanza.syntax in
  plain_string (fun ~loc s -> parse_string_exn ~syntax (loc, s))
;;
