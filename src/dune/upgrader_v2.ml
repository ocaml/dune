open! Stdune
open Upgrader_common

(* If no mode is defined, explicitely use the previous default *)
let explicit_mode fields =
  if Ast_ops.is_in_list [ "modes" ] fields then
    fields
  else
    Dune_lang.Atom.(
      Ast_ops.field_of_list
        [ of_string "modes"; of_string "byte"; of_string "exe" ]) :: fields

(* Field preprocessor_deps cannot exist without field preprocess *)
let no_single_preprocessor_deps fields =
  match Ast_ops.extract_first ["preprocessor_deps"] fields with
  | Some _, rest when Ast_ops.is_in_list
    ["preprocess"; "no_preprocessing"] rest -> rest
  | Some _, rest when Ast_ops.is_in_list ["preprocess"] rest -> fields
  | Some _, rest -> rest
  | None, _ -> fields

(* Always no-op no_keep_loc field should be removed *)
let no_no_keep_loc fields =
  match Ast_ops.extract_first ["no_keep_locs"] fields with
  | Some _, rest | None, rest -> rest

let update_stanza =
  let open Dune_lang.Ast in
    function
    | List (loc, Atom (loca, A "alias") :: tl) as ast ->
      if Ast_ops.is_in_list [ "action" ] tl then
        let tl = Ast_ops.replace_first "name" "alias" tl in
        List (loc, Atom (loca, Dune_lang.Atom.of_string "rule") :: tl)
      else
        ast
    | List (loc, Atom (loca, ((A "executable") as atom)) :: tl)
    | List (loc, Atom (loca, ((A "executables")as atom)) :: tl) ->
      let tl = tl |> no_single_preprocessor_deps |> explicit_mode in
      List (loc, Atom (loca, atom) :: tl)
    | List (loc, Atom (loca, ((A "library") as atom)) :: tl) ->
      let tl = tl |> no_single_preprocessor_deps |> no_no_keep_loc in
      List (loc, Atom (loca, atom) :: tl)
    | stanza -> stanza

let update_formatting sexps =
  let elt, sexps = Ast_ops.extract_first [ "using"; "fmt" ] sexps in
  match elt with
  (* Was not using fmt *)
  | None ->
    sexps
    @ [ Ast_ops.field_of_list
          Dune_lang.Atom.[ of_string "formatting"; of_string "disabled" ]
      ]
  (* Was using fmt *)
  | Some [ _; _; _ ] -> sexps
  (* Was using fmt enabled_for *)
  | Some (_ :: _ :: _ :: tl) ->
    sexps
    @ [ Ast_ops.field_of_list Dune_lang.Atom.[ of_string "formatting" ] ~more:tl
      ]
  (* Unexpected *)
  | _ -> sexps

let update_project_file todo project =
  let sexps, comments =
    Upgrader_common.read_and_parse (Dune_project.file project)
  in
  let v = !Dune_project.default_dune_language_version in
  let sexps = sexps |> Ast_ops.bump_lang_version v |> update_formatting in
  let new_file_contents = string_of_sexps sexps comments in
  (* Printf.eprintf "AAAH: %b\n!" (was_formatted sexps); *)
  todo.to_edit <- (Dune_project.file project, new_file_contents) :: todo.to_edit

let upgrade_dune_file todo fn sexps comments =
  let new_ast =
    sexps
    |> List.map ~f:update_stanza
  in
  let new_file_contents = string_of_sexps new_ast comments in
  todo.to_edit <- (fn, new_file_contents) :: todo.to_edit

let upgrade_dune_files todo dir =
  if String.Set.mem (File_tree.Dir.files dir) File_tree.Dune_file.fname then
    let path = File_tree.Dir.path dir in
    let fn = Path.Source.relative path File_tree.Dune_file.fname in
    let files = Upgrader_common.scan_included_files fn in
    Path.Source.Map.iteri files ~f:(fun fn' (sexps, comments) ->
        upgrade_dune_file todo fn' sexps comments)
