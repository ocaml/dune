open Import

type t =
  | Sexp
  | Csexp

let all = [ "sexp", Sexp; "csexp", Csexp ]

let arg =
  let doc = Printf.sprintf "$(docv) must be %s" (Arg.doc_alts_enum all) in
  Arg.(value & opt (enum all) Sexp & info [ "format" ] ~docv:"FORMAT" ~doc:(Some doc))
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

let name_fields ~split_public_names ~names ~public_names =
  let open Dyn in
  if split_public_names
  then (
    let public_names =
      match public_names with
      | Some public_names -> public_names
      | None -> List.map names ~f:(fun _ -> None)
    in
    [ "names", list string names; "public_names", list (option string) public_names ])
  else (
    let names =
      match public_names with
      | None -> names
      | Some public_names ->
        List.map2 names public_names ~f:(fun name public_name ->
          Option.value public_name ~default:name)
    in
    [ "names", list string names ])
;;

let print_dyn t dyn =
  match t with
  | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn dyn)
  | Sexp -> print_as_sexp dyn
;;
