open Import

let command =
  let doc = "Clean the project." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Removes files added by dune such as _build, <package>.install, and .merlin|}
    ; `P
        {|If one or more PATH arguments are given, only remove those paths from the build directory.|}
    ; `Blocks Common.help_secs
    ]
  in
  let term =
    let+ builder = Common.Builder.term
    and+ paths = Arg.(value & pos_all string [] & info [] ~docv:"PATH" ~doc:None) in
    (* Disable log file creation. Indeed, we are going to delete the whole build directory
       right after and that includes deleting the log file. Not only would creating the
       log file be useless but with some FS this also causes [dune clean] to fail (cf
       https://github.com/ocaml/dune/issues/2964). *)
    let builder = Common.Builder.disable_log_file builder in
    let common, _config = Common.init builder in
    Global_lock.lock_exn ~timeout:None;
    match paths with
    | [] ->
      Dune_engine.Target_promotion.files_in_source_tree_to_delete ()
      |> Path.Source.Set.iter ~f:(fun p -> Fpath.unlink_no_err (Path.Source.to_string p));
      Path.rm_rf Path.build_dir
    | paths ->
      let build_path path =
        let path = Path.of_string (Common.prefix_target common path) in
        match Path.as_in_build_dir path with
        | Some path -> path
        | None ->
          User_error.raise
            [ Pp.textf
                "%s is not inside the build directory"
                (Path.to_string_maybe_quoted path)
            ]
      in
      List.iter paths ~f:(fun path ->
        let path = build_path path in
        let () = Dune_engine.Rule_cache.Workspace_local.remove_target path in
        let () = Dune_engine.Rule_cache.Workspace_local.remove_subtree path in
        Path.rm_rf (Path.build path))
  in
  Cmd.v (Cmd.info "clean" ~doc ~man) term
;;
