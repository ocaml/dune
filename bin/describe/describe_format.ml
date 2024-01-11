open Import

type t =
  | Sexp
  | Csexp

let all = [ "sexp", Sexp; "csexp", Csexp ]

let arg =
  let doc = Printf.sprintf "$(docv) must be %s" (Arg.doc_alts_enum all) in
  Arg.(value & opt (enum all) Sexp & info [ "format" ] ~docv:"FORMAT" ~doc)
;;

let print_as_sexp dyn =
  let rec dune_lang_of_sexp : Sexp.t -> Dune_lang.t = function
    | Atom s -> Dune_lang.atom_or_quoted_string s
    | List l -> List (List.map l ~f:dune_lang_of_sexp)
  in
  let cst =
    dyn
    |> Sexp.of_dyn
    |> dune_lang_of_sexp
    |> Dune_lang.Ast.add_loc ~loc:Loc.none
    |> Dune_lang.Cst.concrete
  in
  let version = Dune_lang.Syntax.greatest_supported_version_exn Stanza.syntax in
  Pp.to_fmt Stdlib.Format.std_formatter (Dune_lang.Format.pp_top_sexps ~version [ cst ])
;;

let print_dyn t dyn =
  match t with
  | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn dyn)
  | Sexp -> print_as_sexp dyn
;;
