open Import
open Memo.O

type t =
  | Dune of Dune_lang.Syntax.Version.t
  | Ocamlformat of Ml_kind.t

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

let source_path path =
  match Path.as_in_source_tree path with
  | Some path -> Some path
  | None -> Path.as_in_build_dir path |> Option.bind ~f:Path.Build.drop_build_context
;;

let for_source ~dir source =
  let* project = Dune_load.find_project ~dir in
  match source_path source with
  | None -> Memo.return None
  | Some source_path ->
    Fs_memo.file_exists (Path.Outside_build_dir.In_source_dir source_path)
    >>= (function
     | false -> Memo.return None
     | true ->
       let+ config = format_config ~dir in
       let name = Path.Source.basename source_path in
       if Filename.equal name Filename.dune || Filename.equal name Filename.dune_project
       then
         Option.some_if
           (Format_config.includes config Format_config.Language.Dune)
           (Dune (Dune_project.dune_version project))
       else
         dialect_formatter config ~dialects:(Dune_project.dialects project) source
         |> Option.bind ~f:(function
           | kind, Dialect.Format.Ocamlformat -> Some (Ocamlformat kind)
           | _, Action _ -> None))
;;

let ocamlformat_action ~expander ~dir ~source ~target kind =
  let open Action_builder.O in
  let cwd = Path.Build.parent_exn target in
  let args =
    [ ocamlformat_flag kind
    ; "--inplace"
    ; "--name"
    ; Path.reach source ~from:(Path.build cwd)
    ; Filename.to_string (Path.Build.basename target)
    ]
  in
  let action program =
    Action.chdir (Path.build cwd) (Action.run program args)
    |> Action.Full.make ~sandbox:Sandbox_config.needs_sandboxing
  in
  let* ocamlformat_is_locked =
    Action_builder.of_memo (ocamlformat_dev_tool_lock_dir_exists ())
  and* () = ocamlformat_config_deps ~dir in
  if ocamlformat_is_locked
  then (
    let path = Path.build (Pkg_dev_tool.exe_path Ocamlformat) in
    let+ () = Action_builder.path path
    and+ env = Action_builder.of_memo (Pkg_rules.dev_tool_env Ocamlformat) in
    action (Ok path) |> Action.Full.add_env env)
  else
    (let open Memo.O in
     let* artifacts = Expander.artifacts expander in
     Artifacts.binary
       artifacts
       ~hint:"opam install ocamlformat"
       ~where:Original_path
       ~dir
       ~loc:None
       (Pkg_dev_tool.exe_name Ocamlformat))
    |> Action_builder.of_memo
    >>| action
;;

let format_diff sctx ~dir ~source ~target ~diff =
  let open Action_builder.O in
  let with_diff formatter =
    Action.Full.map formatter ~f:(fun formatter ->
      Action.progn [ Action.if_file_exists (Path.build target) formatter; diff ])
  in
  Action_builder.of_memo (for_source ~dir source)
  >>= function
  | None -> Action_builder.return (Action.Full.make diff)
  | Some (Dune version) ->
    Format_dune_file.action_stdout ~version (Path.build target)
    |> Action.with_stdout_to_atomic target
    |> Action.Full.make
    |> with_diff
    |> Action_builder.return
  | Some (Ocamlformat kind) ->
    let* expander = Action_builder.of_memo (Super_context.expander sctx ~dir) in
    let+ formatter = ocamlformat_action ~expander ~dir ~source ~target kind in
    with_diff formatter
;;
