open! Stdune
open Upgrader_common

let update_project_file todo project =
    let sexps, comments =
      Upgrader_common.read_and_parse
        (Dune_project.file project)
    in
    let v = !Dune_project.default_dune_language_version in
    let sexps = sexps
      |> Ast_ops.bump_lang_version v
    in
    let new_file_contents = string_of_sexps sexps comments in
    todo.to_edit <-
      (Dune_project.file project, new_file_contents)::todo.to_edit

let upgrade_dune_file todo fn sexps comments =
    let new_ast = sexps
      |> List.map  ~f:Ast_ops.alias_to_rule
    in
    let new_file_contents = string_of_sexps new_ast comments in
    todo.to_edit <- (fn, new_file_contents) :: todo.to_edit


let upgrade_dune_files todo dir =
  if String.Set.mem (File_tree.Dir.files dir) File_tree.Dune_file.fname
  then
    let path = File_tree.Dir.path dir in
    let fn = Path.Source.relative path File_tree.Dune_file.fname in
    let files = Upgrader_common.scan_included_files fn in
    Path.Source.Map.iteri files ~f:(fun fn' (sexps, comments) ->
        upgrade_dune_file todo fn' sexps comments)
