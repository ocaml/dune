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

let input_contents = function
  | Some path -> Io.read_file path
  | None ->
    Exn.protect
      ~f:(fun () ->
        match Fs_io.read_all_unless_large stdin with
        | Ok contents -> contents
        | Error exn -> raise exn)
      ~finally:(fun () -> close_in_noerr stdin)
;;

let format_via_rpc ~builder ~config ~lock_held_by ~path ~input =
  let contents = input_contents input in
  match Dune_lang.Format.parse (Lexing.from_string contents) with
  | OCaml_syntax loc ->
    (match input with
     | None -> User_error.raise ~loc [ Pp.text "OCaml syntax is not supported." ]
     | Some _ -> print_string contents)
  | Sexps _ ->
    Scheduler_setup.no_build_no_rpc ~config (fun () ->
      let open Fiber.O in
      let+ formatted =
        Rpc.Rpc_common.fire_request
          ~name:"format-dune-file"
          ~wait:false
          ~warn_forwarding:false
          ~lock_held_by
          builder
          Dune_rpc.Procedures.Public.format_dune_file
          (path, `Contents contents)
      in
      print_string formatted)
;;

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
    Arg.(value & pos 0 (some file) None & info [] ~docv ~doc:(Some doc))
  and+ version =
    let docv = "VERSION" in
    let doc = "Which version of Dune language to use." in
    Arg.(value & opt (some version) None & info [ "dune-version" ] ~docv ~doc:(Some doc))
  and+ builder = Common.Builder.term in
  let action =
    match version with
    | Some version -> `Format_locally version
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
       | None ->
         `Format_locally
           (Dune_lang.Syntax.greatest_supported_version_exn Dune_lang.Stanza.syntax)
       | Some root ->
         let _common, config = Common.init_with_root ~root builder in
         let input = Option.map ~f:Path.of_filename_relative_to_initial_cwd path_opt in
         (match Global_lock.lock () with
          | Error lock_held_by ->
            let path =
              match input with
              | Some path -> Path.to_absolute_filename path
              | None -> Filename.concat root.dir root.reach_from_root_prefix
            in
            format_via_rpc ~builder ~config ~lock_held_by ~path ~input;
            `Done
          | Ok () ->
            let version =
              Scheduler_setup.no_build_no_rpc ~config
              @@ fun () ->
              Memo.run
              @@
              let open Memo.O in
              let+ dir =
                match
                  Path.as_in_source_tree (Path.of_string root.reach_from_root_prefix)
                with
                | None -> Source_tree.root ()
                | Some path -> Source_tree.nearest_dir path
              in
              Dune_project.dune_version (Source_tree.Dir.project dir)
            in
            `Format_locally version))
  in
  match action with
  | `Done -> ()
  | `Format_locally version ->
    let input = Option.map ~f:Path.of_filename_relative_to_initial_cwd path_opt in
    format_file ~version ~input
;;

let command = Cmd.v info term
