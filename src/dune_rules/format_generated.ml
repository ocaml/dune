open Import
open Memo.O

let rec subdirs_until_root dir =
  match Path.parent dir with
  | None -> [ dir ]
  | Some parent -> dir :: subdirs_until_root parent
;;

let ocamlformat_flag = function
  | Ml_kind.Impl -> "--impl"
  | Intf -> "--intf"
;;

let ocamlformat_dev_tool_lock_dir_exists () =
  match Config.get Compile_time.lock_dev_tools with
  | `Enabled -> Memo.return true
  | `Disabled ->
    let path = Lock_dir.dev_tool_external_lock_dir Ocamlformat in
    Fs_memo.dir_exists (Path.Outside_build_dir.External path)
;;

let ocamlformat_config_deps ~dir =
  let names = [ ".ocamlformat"; ".ocamlformat-ignore"; ".ocamlformat-enable" ] in
  subdirs_until_root (Path.build dir)
  |> List.concat_map ~f:(fun dir -> List.map names ~f:(Path.relative dir))
  |> Action_builder.paths_existing
;;

let format_config ~dir =
  let+ value =
    Env_stanza_db.value_opt ~dir ~f:(fun (config : Dune_env.config) ->
      Memo.return config.format_config)
  and+ default =
    Path.Build.drop_build_context_exn dir
    |> Source_tree.nearest_dir
    >>| Source_tree.Dir.project
    >>| Dune_project.format_config
  in
  Option.value value ~default
;;

let dialect_formatter config ~dialects source =
  let open Option.O in
  let* dialect, kind =
    Path.extension source
    |> Filename.Extension.Or_empty.extension
    |> Option.bind ~f:(Dialect.DB.find_by_extension dialects)
  in
  let* () =
    Option.some_if (Format_config.includes config (Dialect (Dialect.name dialect))) ()
  in
  let+ format =
    match Dialect.format dialect kind with
    | Some format -> Some format
    | None ->
      (match Dialect.preprocess dialect kind with
       | None -> Dialect.format Dialect.ocaml kind
       | Some _ -> None)
  in
  kind, format
;;
