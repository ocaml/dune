open! Stdune
open Upgrader_common

let update_project_file todo project =
    let file = Io.read_file (Path.source (Dune_project.file project))
      ~binary:false in
    let update_dune_lang file =
      let re = Dune_re.(seq [str "(lang dune"; non_greedy (rep any); str ")" ]) in
      let by =  sprintf "(lang dune %s)" (Dune_lang.Syntax.Version.to_string
        !Dune_project.default_dune_language_version)
      in
      Dune_re.replace_string
        (Dune_re.compile re)
        ~by
        file
    in
    let file = update_dune_lang file in
    todo.to_edit <-
      (Dune_project.file project, file)::todo.to_edit


let upgrade_file todo dir =
    let fn =
      Path.Source.relative (File_tree.Dir.path dir)
        File_tree.Dune_file.fname
    in
    if Io.with_lexbuf_from_file (Path.source fn) ~f:Dune_lexer.is_script then
      User_warning.emit
        ~loc:(Loc.in_file (Path.source fn))
        [ Pp.text
            "Cannot upgrade this jbuild file as it is using the OCaml syntax."
        ; Pp.text "You need to upgrade it manually."
        ]
    else
      let s = Io.read_file (Path.source fn) in
      let csts =
        Dune_lang.Parser.parse_string s
          ~fname:(Path.Source.to_string fn)
           ~mode:Cst
      in
      let comments_backup = Dune_lang.Cst.extract_comments csts in
      let asts = List.filter_map csts ~f:Dune_lang.Cst.abstract in
      let asts = List.map asts ~f:Ast_ops.alias_to_rule in
      let csts = List.map asts ~f:Dune_lang.Cst.concrete in
      let csts = Dune_lang.Parser.insert_comments csts comments_backup in
      let contents = Format.asprintf "%a@?" Format_dune_lang.pp_top_sexps csts in
      todo.to_edit <- (fn, contents) :: todo.to_edit
