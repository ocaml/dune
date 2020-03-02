open! Stdune
open Upgrader_common

(* If no mode is defined, explicitely use the previous default *)
let explicit_mode fields =
  if Ast_ops.is_in_list [ "modes" ] fields then
    fields
  else
    Dune_lang.Atom.(
      Ast_ops.field_of_list
        [ of_string "modes"; of_string "byte"; of_string "exe" ])
    :: fields

(* Field preprocessor_deps cannot exist without field preprocess *)
let no_single_preprocessor_deps fields =
  match Ast_ops.extract_first [ "preprocessor_deps" ] fields with
  | Some _, rest
    when Ast_ops.is_in_list [ "preprocess"; "no_preprocessing" ] rest ->
    rest
  | Some _, rest when Ast_ops.is_in_list [ "preprocess" ] rest -> fields
  | Some _, rest -> rest
  | None, _ -> fields

(* Always no-op no_keep_loc field should be removed *)
let no_no_keep_loc fields =
  match Ast_ops.extract_first [ "no_keep_locs" ] fields with
  | Some _, rest
  | None, rest ->
    rest

(* c_names, c_flags, cxx_names and cxx_flags -> foreign_stubs *)
let to_foreign_stubs fields =
  let aux lang fields =
    let names, rest = Ast_ops.extract_first [lang ^ "_names"] fields in
    let flags, rest = Ast_ops.extract_first [lang ^ "_flags"] rest in
    match names, flags with
    | None, None -> fields
    | _ -> (Ast_ops.make_foreign_stubs lang names flags)::rest
  in
  fields |> aux "c" |> aux "cxx"

let update_stanza =
  let open Dune_lang.Ast in
  function
  | List (loc, Atom (loca, A "alias") :: tl) as ast ->
    if Ast_ops.is_in_list [ "action" ] tl then
      let tl = Ast_ops.replace_first "name" "alias" tl in
      List (loc, Atom (loca, Dune_lang.Atom.of_string "rule") :: tl)
    else
      ast
  | List (loc, Atom (loca, (A "executable" as atom)) :: tl)
  | List (loc, Atom (loca, (A "executables" as atom)) :: tl) ->
    let tl = tl
      |> no_single_preprocessor_deps
      |> explicit_mode
      |> to_foreign_stubs
    in
    List (loc, Atom (loca, atom) :: tl)
  | List (loc, Atom (loca, (A "library" as atom)) :: tl) ->
    let tl = tl
      |> no_single_preprocessor_deps
      |> no_no_keep_loc
    in
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
  let new_ast = sexps |> List.map ~f:update_stanza in
  let new_file_contents = string_of_sexps new_ast comments in
  todo.to_edit <- (fn, new_file_contents) :: todo.to_edit

let upgrade_dune_files todo dir =
  if String.Set.mem (File_tree.Dir.files dir) File_tree.Dune_file.fname then
    let path = File_tree.Dir.path dir in
    let fn = Path.Source.relative path File_tree.Dune_file.fname in
    if Io.with_lexbuf_from_file (Path.source fn) ~f:Dune_lexer.is_script then
      User_warning.emit
        ~loc:(Loc.in_file (Path.source fn))
        [ Pp.text
            "Cannot upgrade this file as it is using the OCaml syntax."
        ; Pp.text "You need to upgrade it manually."
        ]
    else
      let files = Upgrader_common.scan_included_files fn in
      Path.Source.Map.iteri files ~f:(fun fn' (sexps, comments) ->
          upgrade_dune_file todo fn' sexps comments)

let todo_log =
  {|
- If you use generated dune.inc files you probably should update your generators.
- mli only modules must now be explicitly declared. This was previously a
  warning and is now an error.
- Stop installing the `ocaml-syntax-shims` binary. In order to use
  `future_syntax`, one now need to depend on the `ocaml-syntax-shims`
  package.
- Actions which introduce targets where new targets are forbidden (e.g.
  preprocessing) are now an error instead of a warning.
- Stop installing the `ocaml-syntax-shims` binary. In order to use
  `future_syntax`, one now need to depend on the `ocaml-syntax-shims`
  package.
- Do not put the `<package>.install` files in the source tree unless `-p` or
  `--promote-install-files` is passed on the command line
- Library names are now validated in a strict fashion. Previously, invalid names
  would be allowed for unwrapped libraries
- Stricter validation of file names in `select`. The file names of conditional
  sources must match the prefix and the extension of the resultant filename.
- Modules filtered out from the module list via the Ordered Set Language must
  now be actual modules.
- Stub names are no longer allowed relative paths. This was previously a warning
  and is now an error.
- In `(diff? x y)` action, require `x` to exist and register a
  dependency on that file.
|}
