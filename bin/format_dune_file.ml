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

let format_file_via_rpc ~config ~lock_held_by ~path_opt ~builder =
  Scheduler_setup.no_build_no_rpc ~config (fun () ->
    let open Fiber.O in
    let path, contents =
      match path_opt with
      | Some p ->
        let path = Path.of_filename_relative_to_initial_cwd p in
        Path.to_string path, Io.read_file path
      | None -> Filename.current_dir_name, In_channel.(input_all stdin)
    in
    Rpc.Rpc_common.fire_request
      ~name:"format-dune-file"
      ~wait:false
      ~lock_held_by
      builder
      Dune_rpc.Procedures.Public.format_dune_file
      (path, `Contents contents)
    >>| print_string)
;;

let format_file_locally_with_version ~version ~path_opt =
  let input = Option.map ~f:Path.of_filename_relative_to_initial_cwd path_opt in
  format_file ~version ~input
;;

let detect_version_and_format ~root ~config ~path_opt =
  let version =
    Scheduler_setup.no_build_no_rpc ~config
    @@ fun () ->
    Memo.run
    @@
    let open Memo.O in
    let+ dir =
      match Path.as_in_source_tree (Path.of_string root.Workspace_root.reach_from_root_prefix)
      with
      | None -> Source_tree.root ()
      | Some path -> Source_tree.nearest_dir path
    in
    Dune_project.dune_version (Source_tree.Dir.project dir)
  in
  let input = Option.map ~f:Path.of_filename_relative_to_initial_cwd path_opt in
  format_file ~version ~input
;;

let term =
  let+ path_opt =
    let docv = "FILE" in
    let doc = "Path to the dune file to parse." in
    Arg.(value & pos 0 (some file) None & info [] ~docv ~doc:(Some doc))
  and+ version_opt =
    let docv = "VERSION" in
    let doc = "Which version of Dune language to use." in
    Arg.(value & opt (some version) None & info [ "dune-version" ] ~docv ~doc:(Some doc))
  and+ builder = Common.Builder.term in
  (* If version is explicitly provided, no scheduler needed, format locally *)
  match version_opt with
  | Some version -> format_file_locally_with_version ~version ~path_opt
  | None ->
    (* Need to detect version - check if workspace exists *)
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
       (* No workspace, use greatest supported version *)
       let version =
         Dune_lang.Syntax.greatest_supported_version_exn Dune_lang.Stanza.syntax
       in
       format_file_locally_with_version ~version ~path_opt
     | Some root ->
       (* Workspace found, initialize and check lock *)
       let _common, config = Common.init_with_root ~root builder in
       (match Global_lock.lock ~timeout:None with
        | Ok () -> detect_version_and_format ~root ~config ~path_opt
        | Error lock_held_by -> format_file_via_rpc ~config ~lock_held_by ~path_opt ~builder))
;;

let command = Cmd.v info term
