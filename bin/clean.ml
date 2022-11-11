open Stdune
open Import

let command =
  let doc = "Clean the project." in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|Removes files added by dune such as _build, <package>.install, and .merlin|}
    ; `Blocks Common.help_secs
    ]
  in
  let term =
    let+ common = Common.term in
    (* Pass [No_log_file] to prevent the log file from being created. Indeed, we
       are going to delete the whole build directory right after and that
       includes deleting the log file. Not only creating the log file would be
       useless but with some FS this also causes [dune clean] to fail (cf
       https://github.com/ocaml/dune/issues/2964). *)
    let _config = Common.init common ~log_file:No_log_file in
    Dune_util.Global_lock.lock_exn ~timeout:None;
    Dune_engine.Target_promotion.files_in_source_tree_to_delete ()
    |> Path.Source.Set.iter ~f:(fun p -> Path.unlink_no_err (Path.source p));
    Path.rm_rf Path.build_dir
  in
  Cmd.v (Cmd.info "clean" ~doc ~man) term
