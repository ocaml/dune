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

let dialect_formatter config project source =
  let open Option.O in
  let dialects = Dune_project.dialects project in
  let* dialect, kind =
    Path.extension source
    |> Filename.Extension.Or_empty.extension
    |> Option.bind ~f:(Dialect.DB.find_by_extension dialects)
  in
  let* () =
    Option.some_if (Format_config.includes config (Dialect (Dialect.name dialect))) ()
  in
  let* format =
    match Dialect.format dialect kind with
    | Some format -> Some format
    | None ->
      (match Dialect.preprocess dialect kind with
       | None -> Dialect.format Dialect.ocaml kind
       | Some _ -> None)
  in
  match format with
  | Ocamlformat -> Some (Ocamlformat kind)
  | Action _ -> None
;;

let source_path path =
  match Path.as_in_source_tree path with
  | Some path -> Some path
  | None -> Path.as_in_build_dir path |> Option.bind ~f:Path.Build.drop_build_context
;;

let for_source ~dir source =
  let* project = Dune_load.find_project ~dir in
  let version = Dune_project.dune_version project in
  if version < (3, 25)
  then Memo.return None
  else (
    match source_path source with
    | None -> Memo.return None
    | Some source_path ->
      let* source_exists =
        Fs_memo.file_exists (Path.Outside_build_dir.In_source_dir source_path)
      in
      if not source_exists
      then Memo.return None
      else
        let+ config = format_config ~dir in
        if Filename.equal (Path.Source.basename source_path) Filename.dune
        then
          Option.some_if
            (Format_config.includes config Format_config.Language.Dune)
            (Dune version)
        else dialect_formatter config project source)
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
    |> Action.Full.make
    |> Action.Full.add_sandbox Sandbox_config.needs_sandboxing
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
  else (
    let program =
      let open Memo.O in
      let* artifacts = Expander.artifacts expander in
      Artifacts.binary
        artifacts
        ~hint:"opam install ocamlformat"
        ~where:Original_path
        ~dir
        ~loc:None
        (Pkg_dev_tool.exe_name Ocamlformat)
    in
    let+ program = Action_builder.of_memo program in
    action program)
;;

let action sctx ~dir ~source ~target =
  let open Action_builder.O in
  Action_builder.of_memo (for_source ~dir source)
  >>= function
  | None -> Action_builder.return Action.Full.empty
  | Some (Dune version) ->
    Format_dune_file.action ~version (Path.build target) target
    |> Action.Full.make
    |> Action_builder.return
  | Some (Ocamlformat kind) ->
    let* expander = Action_builder.of_memo (Super_context.expander sctx ~dir) in
    ocamlformat_action ~expander ~dir ~source ~target kind
;;

let format_diff sctx ~dir ~source ~target ~diff =
  let open Action_builder.O in
  let+ formatter = action sctx ~dir ~source ~target in
  Action.Full.reduce [ Action.Full.make Action.empty; formatter ]
  |> Action.Full.map ~f:(fun formatter ->
    Action.progn [ Action.if_file_exists (Path.build target) formatter; diff ])
;;
