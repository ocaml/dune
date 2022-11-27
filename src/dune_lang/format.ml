open! Stdune
open Dune_sexp
open Pp.O
module Format = Stdlib.Format

type dune_file =
  | OCaml_syntax of Loc.t
  | Sexps of Cst.t list

let parse lb =
  if Dune_file_script.is_script lb then OCaml_syntax (Loc.of_lexbuf lb)
  else Sexps (Parser.parse lb ~mode:Cst)

let can_be_displayed_wrapped =
  List.for_all ~f:(fun (c : Cst.t) ->
      match c with
      | Atom _ | Quoted_string _ | Template _ | List (_, []) | List (_, [ _ ])
        -> true
      | List _ | Comment _ -> false)

let pp_simple t =
  Cst.abstract t |> Option.value_exn |> Ast.remove_locs |> Dune_sexp.pp

let print_wrapped_list ~version x =
  let inner = Pp.concat_map ~sep:Pp.space ~f:pp_simple x in
  if version < (2, 8) then
    Pp.char '(' ++ Pp.hovbox ~indent:1 inner ++ Pp.char ')'
  else Pp.hvbox ~indent:1 (Pp.char '(' ++ inner ++ Pp.char ')')

let pp_comment_line l = Pp.char ';' ++ Pp.verbatim l

let pp_comment lines =
  Pp.vbox (Pp.concat_map ~sep:Pp.cut ~f:pp_comment_line lines)

let pp_break attached = if attached then Pp.char ' ' else Pp.cut

let pp_list_with_comments pp_sexp sexps =
  let rec go (l : Cst.t list) =
    match l with
    | x :: Comment (loc, c) :: xs ->
      let attached = Loc.on_same_line (Cst.loc x) loc in
      pp_sexp x ++ pp_break attached ++ pp_comment c ++ Pp.cut ++ go xs
    | Comment (_, c) :: xs -> pp_comment c ++ Pp.cut ++ go xs
    | [ x ] -> pp_sexp x
    | x :: xs -> pp_sexp x ++ Pp.cut ++ go xs
    | [] -> Pp.nop
  in
  go sexps

let rec pp_sexp ~version : Cst.t -> _ = function
  | (Atom _ | Quoted_string _ | Template _) as sexp -> pp_simple sexp
  | List (_, sexps) ->
    Pp.vbox ~indent:1
      (if can_be_displayed_wrapped sexps then print_wrapped_list ~version sexps
      else pp_sexp_list ~version sexps)
  | Comment (_, c) -> pp_comment c

and pp_sexp_list ~version sexps =
  Pp.char '(' ++ pp_list_with_comments (pp_sexp ~version) sexps ++ Pp.char ')'

let pp_top_sexp ~version sexp = pp_sexp ~version sexp ++ Pp.char '\n'

let pp_top_sexps ~version =
  Pp.concat_map ~sep:Pp.newline ~f:(pp_top_sexp ~version)

let format_string ~version input =
  match parse (Lexing.from_string input) with
  | OCaml_syntax _ ->
    User_error.raise [ Pp.text "OCaml syntax is not supported." ]
  | Sexps sexps ->
    Format.asprintf "%a%!" Pp.to_fmt (pp_top_sexps ~version sexps)

let format_action ~version ~src ~dst =
  let dst = Path.build dst in
  match Io.with_lexbuf_from_file src ~f:parse with
  | OCaml_syntax _ -> Io.copy_file ~src ~dst ()
  | Sexps sexps ->
    Io.with_file_out dst ~f:(fun oc ->
        let oc = Format.formatter_of_out_channel oc in
        Format.fprintf oc "%a%!" Pp.to_fmt (pp_top_sexps ~version sexps))
