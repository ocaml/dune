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
