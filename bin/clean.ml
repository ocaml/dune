open Import

(* Preserve package management data by default. See #12976 *)
let preserved_context_dirs = [ ".pkg"; ".dev-tool" ]
let preserved_build_dirs = [ ".dev-tools.locks" ]

let clear_context_dir path =
  match Path.readdir_unsorted_with_kinds path with
  | Error _ -> ()
  | Ok entries ->
    List.iter entries ~f:(fun (name, kind) ->
      if not (List.mem preserved_context_dirs name ~equal:String.equal)
      then (
        let p = Path.relative path name in
        match kind with
        | Unix.S_DIR -> Path.rm_rf p
        | _ -> Fpath.unlink_no_err (Path.to_string p)))
;;

let clear_private_dir path =
  match Path.readdir_unsorted_with_kinds path with
  | Error _ -> ()
  | Ok entries ->
    List.iter entries ~f:(fun (name, kind) ->
      let p = Path.relative path name in
      match kind with
      | Unix.S_DIR -> clear_context_dir p
      | _ -> Fpath.unlink_no_err (Path.to_string p))
;;

let clean_preserving_packages () =
  if Path.exists Path.build_dir
  then (
    match Path.readdir_unsorted_with_kinds Path.build_dir with
    | Error _ -> Path.rm_rf Path.build_dir
    | Ok entries ->
      List.iter entries ~f:(fun (name, kind) ->
        let p = Path.relative Path.build_dir name in
        match kind with
        | Unix.S_DIR ->
          if List.mem preserved_build_dirs name ~equal:String.equal
          then ()
          else if String.equal name "_private"
          then clear_private_dir p
          else clear_context_dir p
        | _ -> Fpath.unlink_no_err (Path.to_string p)))
;;

let command =
  let doc = "Clean the project." in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|Removes files added by dune such as _build, <package>.install, and .merlin.

By default, package management data (.pkg and .dev-tool directories) is preserved
to avoid expensive rebuilds. Use --full to remove everything including packages.|}
    ; `Blocks Common.help_secs
    ]
  in
  let term =
    let+ builder = Common.Builder.term
    and+ full =
      Arg.(
        value
        & flag
        & info
            [ "full" ]
            ~doc:
              (Some
                 "Remove everything including package management data. By default, the \
                  .pkg and .dev-tool directories are preserved."))
    in
    (* Disable log file creation since we're about to delete the build directory.
       See https://github.com/ocaml/dune/issues/2964 *)
    let builder = Common.Builder.disable_log_file builder in
    let _common, _config = Common.init builder in
    Dune_util.Global_lock.lock_exn ~timeout:None;
    Dune_engine.Target_promotion.files_in_source_tree_to_delete ()
    |> Path.Source.Set.iter ~f:(fun p -> Fpath.unlink_no_err (Path.Source.to_string p));
    if full then Path.rm_rf Path.build_dir else clean_preserving_packages ()
  in
  Cmd.v (Cmd.info "clean" ~doc ~man) term
;;
