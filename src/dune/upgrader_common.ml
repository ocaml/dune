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

  let rec replace_first old_name new_name = function
  | List(loc, Atom(loca, A atom) :: tll)
    :: tl when atom = old_name ->
      List(loc, Atom(loca, Dune_lang.Atom.of_string new_name) :: tll)
      :: tl
  | hd :: tl -> hd :: (replace_first old_name new_name tl)
  | [] -> []

  let alias_to_rule = function
    | List
        (loc,
          Atom (loca, A "alias") :: tl
        ) ->
        let tl = replace_first "name" "alias" tl in
        List (loc, Atom (loca, Dune_lang.Atom.of_string ("rule")) :: tl)
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

(* Return a mapping [Path.t -> Dune_lang.Ast.t list] containing [path] and all
   the files in includes, recursiverly *)
let scan_included_files ?lexer path =
  let files = ref Path.Source.Map.empty in
  let rec iter path =
    if not (Path.Source.Map.mem !files path) then (
      let s = Io.read_file (Path.source path) in
      let csts =
        Dune_lang.Parser.parse_string s
          ~fname:(Path.Source.to_string path)
          ?lexer ~mode:Cst
        |> List.map ~f:(Dune_lang.Cst.fetch_legacy_comments ~file_contents:s)
      in
      let comments = Dune_lang.Cst.extract_comments csts in
      let sexps = List.filter_map csts ~f:Dune_lang.Cst.abstract in
      files := Path.Source.Map.set !files path (sexps, comments);
      List.iter (Ast_ops.included_files_paths path sexps) ~f:iter
    )
  in
  iter path;
  !files
