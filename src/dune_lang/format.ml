open Import
open Pp.O
module Format = Stdlib.Format

type dune_file =
  | OCaml_syntax of Loc.t
  | Sexps of Cst.t list

let parse lb =
  if Dune_file_script.is_script lb
  then OCaml_syntax (Loc.of_lexbuf lb)
  else Sexps (Parser.parse lb ~mode:Cst)
;;

let block_string_min_version = 3, 25

let can_be_displayed_wrapped ~version =
  List.for_all ~f:(fun (sexp : Cst.t) ->
    match sexp with
    | Atom _ | List (_, []) | List (_, [ _ ]) | Template _ | Quoted_string _ -> true
    | Block_string _ -> version < block_string_min_version
    | List _ | Comment _ -> false)
;;

let pp_simple t = Cst.abstract t |> Option.value_exn |> Ast.remove_locs |> Dune_sexp.pp

let escaped_block_line parts =
  String.concat
    ~sep:""
    (List.map parts ~f:(function
       | Template.Part.Text text -> Escape.escaped text
       | Pform pform -> Template.Pform.to_string pform))
;;

let pp_block_line (kind, parts) =
  let kind, contents =
    match kind with
    | Block_string.Kind.Escaped -> kind, escaped_block_line parts
    | Raw ->
      if
        List.for_all parts ~f:(function
          | Template.Part.Text text ->
            not (String.exists text ~f:(fun char -> char = '\r' || char = '\n'))
          | Pform _ -> false)
      then
        ( kind
        , String.concat
            ~sep:""
            (List.map parts ~f:(function
               | Template.Part.Text text -> text
               | Pform pform -> Template.Pform.to_string pform)) )
      else Escaped, escaped_block_line parts
  in
  let prefix =
    match kind with
    | Block_string.Kind.Escaped -> "\"\\|"
    | Raw -> "\"\\>"
  in
  Pp.verbatim
    (if String.is_empty contents then prefix else Printf.sprintf "%s %s" prefix contents)
;;

let preserved_block_lines ~version lines =
  if version < block_string_min_version
  then None
  else (
    match List.rev lines with
    | (_, [] | _, [ Template.Part.Text "" ]) :: (_ :: _ as lines) -> Some (List.rev lines)
    | _ -> None)
;;

(* Compute block preservation once so nested lists do not repeatedly scan their
   descendants while deciding how to format them. *)
module Annotated = struct
  type t =
    { cst : Cst.t
    ; children : t list
    ; block_lines : Block_string.t option
    ; contains_preserved_block : bool
    }
end

let rec annotate ~version (cst : Cst.t) =
  let children, block_lines =
    match cst with
    | List (_, sexps) -> List.map sexps ~f:(annotate ~version), None
    | Block_string (_, lines) -> [], preserved_block_lines ~version lines
    | Atom _ | Quoted_string _ | Template _ | Comment _ -> [], None
  in
  let contains_preserved_block =
    Option.is_some block_lines
    || List.exists children ~f:(fun { Annotated.contains_preserved_block; _ } ->
      contains_preserved_block)
  in
  { Annotated.cst; children; block_lines; contains_preserved_block }
;;

let sexp_ends_with_cut { Annotated.block_lines; _ } = Option.is_some block_lines
let pp_comment_line l = Pp.char ';' ++ Pp.verbatim l
let pp_comment lines = Pp.vbox (Pp.concat_map ~sep:Pp.cut ~f:pp_comment_line lines)
let pp_break attached = if attached then Pp.char ' ' else Pp.cut

let rec pp_sexp ~version : Annotated.t -> _ = function
  | { cst = Quoted_string (_, string); _ } -> Pp.verbatim (Escape.quoted string)
  | { cst = Block_string _ as sexp; block_lines; _ } ->
    (match block_lines with
     | Some lines -> Pp.vbox (Pp.concat_map lines ~sep:Pp.cut ~f:pp_block_line ++ Pp.cut)
     | None -> pp_simple sexp)
  | { cst = (Atom _ | Template _) as sexp; _ } -> pp_simple sexp
  | { cst = List (_, sexps); children; _ } ->
    Pp.vbox
      ~indent:1
      (if can_be_displayed_wrapped ~version sexps
       then print_wrapped_list ~version children
       else pp_sexp_list ~version children)
  | { cst = Comment (_, comment); _ } -> pp_comment comment

and pp_sexp_list ~version sexps =
  Pp.char '(' ++ pp_list_with_comments ~version sexps ++ Pp.char ')'

and pp_list_with_comments ~version = function
  | ({ Annotated.cst = sexp_cst; _ } as sexp)
    :: { cst = Comment (loc, comment); _ }
    :: sexps ->
    let break =
      if sexp_ends_with_cut sexp
      then Pp.nop
      else pp_break (Loc.on_same_line (Cst.loc sexp_cst) loc)
    in
    pp_sexp ~version sexp
    ++ break
    ++ pp_comment comment
    ++ Pp.cut
    ++ pp_list_with_comments ~version sexps
  | { cst = Comment (_, comment); _ } :: sexps ->
    pp_comment comment ++ Pp.cut ++ pp_list_with_comments ~version sexps
  | [ sexp ] -> pp_sexp ~version sexp
  | sexp :: (next :: _ as sexps) ->
    let break =
      if sexp_ends_with_cut sexp
      then if sexp_ends_with_cut next then Pp.cut else Pp.nop
      else Pp.cut
    in
    pp_sexp ~version sexp ++ break ++ pp_list_with_comments ~version sexps
  | [] -> Pp.nop

and print_wrapped_list ~version sexps =
  let inner =
    Pp.concat_map
      sexps
      ~sep:Pp.space
      ~f:(fun ({ Annotated.cst; contains_preserved_block; _ } as sexp) ->
        match cst with
        | Quoted_string _ | Block_string _ -> pp_sexp ~version sexp
        | List _ when contains_preserved_block -> pp_sexp ~version sexp
        | Atom _ | Template _ | List _ -> pp_simple cst
        | Comment _ -> Code_error.raise "unexpected comment in a wrapped list" [])
  in
  if version < (2, 8)
  then Pp.char '(' ++ Pp.hovbox ~indent:1 inner ++ Pp.char ')'
  else Pp.hvbox ~indent:1 (Pp.char '(' ++ inner ++ Pp.char ')')
;;

let pp_top_sexp ~version sexp =
  pp_sexp ~version sexp ++ if sexp_ends_with_cut sexp then Pp.nop else Pp.char '\n'
;;

let pp_top_sexps ~version sexps =
  Pp.vbox
    (Pp.concat_map sexps ~sep:Pp.cut ~f:(fun sexp ->
       pp_top_sexp ~version (annotate ~version sexp)))
;;

let format_string ~version input =
  match parse (Lexing.from_string input) with
  | OCaml_syntax _ -> User_error.raise [ Pp.text "OCaml syntax is not supported." ]
  | Sexps sexps -> Format.asprintf "%a%!" Pp.to_fmt (pp_top_sexps ~version sexps)
;;

let format_to_channel ~version ~src oc =
  match Io.with_lexbuf_from_file src ~f:parse with
  | OCaml_syntax _ -> Io.with_file_in src ~f:(fun ic -> Io.copy_channels ic oc)
  | Sexps sexps ->
    let oc = Format.formatter_of_out_channel oc in
    Format.fprintf oc "%a%!" Pp.to_fmt (pp_top_sexps ~version sexps)
;;

let format_action ~version ~src ~dst =
  Path.build dst |> Io.with_file_out ~f:(format_to_channel ~version ~src)
;;
