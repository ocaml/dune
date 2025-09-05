open Import

let doc = "Format dune files."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune format-dune-file) reads a dune file and outputs a formatted
           version. This is a low-level command, meant to implement editor
           support for example. To reformat a dune project, see the "Automatic
           formatting" section in the manual.|}
  ]
;;

let info = Cmd.info "format-dune-file" ~doc ~man

let format_file ~version ~input =
  let with_input =
    match input with
    | Some path -> fun f -> Io.with_lexbuf_from_file path ~f
    | None ->
      fun f ->
        Exn.protect
          ~f:(fun () -> f (Lexing.from_channel stdin))
          ~finally:(fun () -> close_in_noerr stdin)
  in
  match with_input Dune_lang.Format.parse with
  | Sexps sexps ->
    Format.fprintf
      Format.std_formatter
      "%a%!"
      Pp.to_fmt
      (Dune_lang.Format.pp_top_sexps ~version sexps)
  | OCaml_syntax loc ->
    (match input with
     | None -> User_error.raise ~loc [ Pp.text "OCaml syntax is not supported." ]
     | Some path -> Io.with_file_in path ~f:(fun ic -> Io.copy_channels ic stdout))
;;

let term =
  let+ path_opt =
    let docv = "FILE" in
    let doc = "Path to the dune file to parse." in
    Arg.(value & pos 0 (some file) None & info [] ~docv ~doc)
  and+ version =
    let docv = "VERSION" in
    let doc = "Which version of Dune language to use." in
    Arg.(value & opt (some version) None & info [ "dune-version" ] ~docv ~doc)
  and+ builder = Common.Builder.term in
  let version =
    match version with
    | Some version -> version
    | None ->
      let from =
        match path_opt with
        | None -> Filename.current_dir_name
        | Some path -> Filename.dirname path
      in
      (match
         Workspace_root.create
           ~from
           ~default_is_cwd:(Common.Builder.default_root_is_cwd builder)
           ~specified_by_user:(Common.Builder.root builder)
           ()
       with
       | None -> Dune_lang.Syntax.greatest_supported_version_exn Dune_lang.Stanza.syntax
       | Some root ->
         let common, config = Common.init_with_root ~root builder in
         Scheduler.go_with_rpc_server ~common ~config
         @@ fun () ->
         Memo.run
         @@
         let open Memo.O in
         let+ dir =
           match Path.as_in_source_tree (Path.of_string root.reach_from_root_prefix) with
           | None -> Source_tree.root ()
           | Some path -> Source_tree.nearest_dir path
         in
         Dune_project.dune_version (Source_tree.Dir.project dir))
  in
  let input = Option.map ~f:Path.of_filename_relative_to_initial_cwd path_opt in
  format_file ~version ~input
;;

let command = Cmd.v info term
