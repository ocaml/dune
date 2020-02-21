open! Stdune

type rename_and_edit =
  { original_file : Path.Source.t
  ; extra_files_to_delete : Path.Source.t list
  ; new_file : Path.Source.t
  ; contents : string
  }

type todo =
  { mutable to_rename_and_edit : rename_and_edit list
  ; mutable to_edit : (Path.Source.t * string) list
  }

module Ast_ops = struct
  open Dune_lang.Ast

  let field_of_list ?more:(m=[]) atoms =
    List(Loc.none, (List.map atoms
      ~f:(fun a -> Atom(Loc.none, a)))
      @ m
      )

  let rec replace_first old_name new_name = function
  | List(loc, Atom(loca, A atom) :: tll)
    :: tl when atom = old_name ->
      List(loc, Atom(loca, Dune_lang.Atom.of_string new_name) :: tll) :: tl
  | List(loc, Quoted_string(loca, str) :: tll)
    :: tl when str = old_name ->
      List(loc, Quoted_string(loca, new_name) :: tll) :: tl
  | hd :: tl -> hd :: (replace_first old_name new_name tl)
  | [] -> []

  let extract_first names =
    let rec is_names vals names = match vals, names with
      | (Atom(_, A str) | Quoted_string(_, str)) :: tl, name :: tln
        when str = name -> is_names tl tln
      | _, [] -> true
      | _, _ -> false
    in
    let rec aux rest = function
    | List(_, elt) :: _ when is_names elt names -> Some elt, List.rev rest
    | hd :: tl -> aux (hd :: rest) tl
    | [] -> None, List.rev rest
    in aux []

  let is_in_list names sexp =
    match fst (extract_first names sexp) with
    | Some _ -> true
    | None -> false

  let bump_lang_version v =
    let v = Dune_lang.Syntax.Version.to_string v in
    function
    | List(loc, (Atom(_, A "lang") as lang)
        :: (Atom(_, A "dune")  as dune)
        :: Atom(loc3, A _)
        :: tll) ::tl ->
      List(loc, lang :: dune
        :: Atom(loc3, Dune_lang.Atom.of_string v)
        :: tll) :: tl
    | sexp -> sexp

  let alias_to_rule = function
    | List
        (loc,
          Atom (loca, A "alias") :: tl
        ) as ast ->
        if is_in_list ["action"] tl then
          let tl = replace_first "name" "alias" tl in
          List (loc, Atom (loca, Dune_lang.Atom.of_string ("rule")) :: tl)
        else ast
    | ast -> ast

  let included_file path = function
    | List
            ( _
            , [ Atom (_, A "include")
              ; (Atom (loc, A fn) | Quoted_string (loc, fn))
              ] ) ->
          let dir = Path.Source.parent_exn path in
          let included_file = Path.Source.relative dir fn in
          if not (Path.exists (Path.source included_file)) then
            User_error.raise ~loc
              [ Pp.textf "File %s doesn't exist."
                  (Path.Source.to_string_maybe_quoted included_file)
              ];
          Some included_file
    | _ -> None

  let included_files_paths path = List.filter_map ~f:(included_file path)
end

let read_and_parse ?lexer path =
  let raw = Io.read_file (Path.source path) ~binary:false in
  let csts =
        Dune_lang.Parser.parse_string raw
          ~fname:(Path.Source.to_string path)
          ?lexer ~mode:Cst
        |> List.map
          ~f:(Dune_lang.Cst.fetch_legacy_comments ~file_contents:raw)
      in
  let comments = Dune_lang.Cst.extract_comments csts in
  let sexps = List.filter_map csts ~f:Dune_lang.Cst.abstract in
  sexps, comments

(* Return a mapping [Path.t -> Dune_lang.Ast.t list] containing [path] and all
   the files in includes, recursiverly *)
let scan_included_files ?lexer path =
  let files = ref Path.Source.Map.empty in
  let rec iter path =
    if not (Path.Source.Map.mem !files path) then (
      let sexps, comments = read_and_parse ?lexer path in
      files := Path.Source.Map.set !files path (sexps, comments);
      List.iter (Ast_ops.included_files_paths path sexps) ~f:iter
    )
  in
  iter path;
  !files

let string_of_sexps sexps comments =
  let new_csts = List.map sexps ~f:Dune_lang.Cst.concrete in
  Dune_lang.Parser.insert_comments new_csts comments
  |> Format.asprintf "%a@?" Format_dune_lang.pp_top_sexps
