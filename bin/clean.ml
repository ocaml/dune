open Import

let command =
  let doc = "Clean the project." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Removes files added by dune such as _build, <package>.install, and .merlin|}
    ; `Blocks Common.help_secs
    ]
  in
  let term =
    let+ builder = Common.Builder.term in
    (* Disable log file creation. Indeed, we are going to delete the whole build directory
       right after and that includes deleting the log file. Not only would creating the
       log file be useless but with some FS this also causes [dune clean] to fail (cf
       https://github.com/ocaml/dune/issues/2964). *)
    let builder = Common.Builder.disable_log_file builder in
    let _common, _config = Common.init builder in
    Dune_util.Global_lock.lock_exn ~timeout:None;
    Dune_engine.Target_promotion.files_in_source_tree_to_delete ()
    |> Path.Source.Set.iter ~f:(fun p -> Path.unlink_no_err (Path.source p));
    Path.rm_rf Path.build_dir
  in
  Cmd.v (Cmd.info "clean" ~doc ~man) term
;;
