open Import

let command =
  let doc = "Clean the project." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Removes files added by dune such as _build, <package>.install, and .merlin|}
    ; `P
        {|If one or more PATH arguments are given, only those paths are removed from the build directory. A PATH may be a path inside the build directory (e.g. _build/default/foo) or a source directory (e.g. foo), in which case the corresponding artifacts are removed from every build context.|}
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
    let common, config = Common.init builder in
    Global_lock.lock_exn ();
    match paths with
    | [] ->
      Dune_engine.Target_promotion.files_in_source_tree_to_delete ()
      |> Path.Source.Set.iter ~f:(fun p -> Fpath.unlink_no_err (Path.Source.to_string p));
      Path.rm_rf Path.build_dir
    | paths ->
      let remove_build_path path =
        Dune_engine.Rule_cache.Workspace_local.remove_target path;
        Dune_engine.Rule_cache.Workspace_local.remove_subtree path;
        Path.rm_rf (Path.build path)
      in
      (* Paths already inside the build directory are removed as is. Source paths
         (including the project root) are resolved against every build context,
         mirroring how [dune build] accepts source directories. *)
      let build_paths, source_paths =
        List.partition_map paths ~f:(fun path ->
          let path = Path.of_string (Common.prefix_target common path) in
          match Path.as_in_build_dir path with
          | Some path -> Left path
          | None ->
            (match Path.as_in_source_tree path with
             | Some src -> Right src
             | None ->
               User_error.raise
                 [ Pp.textf
                     "%s is not inside the build directory or the source tree"
                     (Path.to_string_maybe_quoted path)
                 ]))
      in
      let source_build_paths =
        match source_paths with
        | [] -> []
        | _ :: _ ->
          Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
            Memo.run
              (let open Memo.O in
               let+ contexts = Context.DB.all () in
               List.concat_map source_paths ~f:(fun src ->
                 List.map contexts ~f:(fun ctx ->
                   Path.Build.append_source (Context.build_dir ctx) src))))
      in
      List.iter (build_paths @ source_build_paths) ~f:remove_build_path
  in
  Cmd.v (Cmd.info "clean" ~doc ~man) term
;;
