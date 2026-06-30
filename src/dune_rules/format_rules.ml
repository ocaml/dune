open Import
open Memo.O

let rec subdirs_until_root dir =
  match Path.parent dir with
  | None -> [ dir ]
  | Some d -> dir :: subdirs_until_root d
;;

let depend_on_files ~named dir =
  subdirs_until_root dir
  |> List.concat_map ~f:(fun dir -> List.map named ~f:(Path.relative dir))
  |> Action_builder.paths_existing
;;

let formatter_diff_action =
  let dep_on_alias_action alias ~loc action =
    let action =
      let open Action_builder.O in
      let+ action = action in
      { Rule.Anonymous_action.action
      ; loc
      ; dir = Alias.dir alias
      ; alias = Some (Alias.name alias)
      }
    in
    Build_system.dep_on_alias_definition (Rules.Dir_rules.Alias_spec.Action action)
  in
  let formatter_stdout sctx ~loc alias action =
    let dir = Alias.dir alias in
    let action =
      let open Action_builder.O in
      let+ (action : Action.Full.t) = action
      and+ env =
        Super_context.env_node sctx ~dir
        |> Memo.bind ~f:Env_node.external_env
        |> Action_builder.of_memo
      in
      let env = Env_path.extend_env_concat_path env action.env in
      let action = Action.Full.add_env env action in
      { Rule.Anonymous_action.action; loc; dir; alias = Some (Alias.name alias) }
    in
    Build_system.execute_action_stdout action |> Action_builder.of_memo
  in
  fun sctx ~loc alias ~input formatter ->
    let open Action_builder.O in
    let action =
      let+ formatter = formatter_stdout sctx ~loc alias formatter
      and+ () = Action_builder.path (Path.build input) in
      Action.chdir
        (Path.build (Path.Build.parent_exn input))
        (let output = Path.Build.extend_basename input ~suffix:Filename.corrected in
         Action.progn
           [ Action.write_file output formatter; Action.diff (Path.build input) output ])
      |> Action.Full.make
      |> Action.Full.add_sandbox Sandbox_config.needs_sandboxing
    in
    dep_on_alias_action alias ~loc action
;;

module Ocamlformat = struct
  let dev_tool_lock_dir_exists () =
    (* we assume that if lock_dev_tools is set, then the lock dir was created
       via locking and can expect it to exist. If it doesn't, it's a bug
    *)
    match Config.get Compile_time.lock_dev_tools with
    | `Enabled -> Memo.return true
    | `Disabled ->
      (* even if lock_dev_tools might be disabled, there might be a lock dir
         created by `dune tools install` *)
      let path = Lock_dir.dev_tool_external_lock_dir Ocamlformat in
      Fs_memo.dir_exists (Path.Outside_build_dir.External path)
  ;;

  (* Config files for ocamlformat. When these are changed, running
     `dune fmt` should cause ocamlformat to re-format the ocaml files
     in the project. *)
  let config_files = [ ".ocamlformat"; ".ocamlformat-ignore"; ".ocamlformat-enable" ]

  let extra_deps dir =
    (* Set up the dependency on ocamlformat config files so changing
       these files triggers ocamlformat to run again. *)
    depend_on_files ~named:config_files (Path.build dir)
  ;;

  let flag_of_kind = function
    | Ml_kind.Impl -> "--impl"
    | Intf -> "--intf"
  ;;

  let action_when_ocamlformat_is_locked ~input kind =
    let open Action_builder.O in
    let dir = Path.Build.parent_exn input in
    let action =
      let path = Path.build @@ Pkg_dev_tool.exe_path Ocamlformat in
      (* This ensures that ocamlformat is installed as a dev tool before
         running it. *)
      let+ () = Action_builder.path path
      (* Declare the dependency on the input file so changes to the input file
         trigger ocamlformat to run again on the updated file. *)
      and+ () = Action_builder.path (Path.build input) in
      Action.chdir
        (Path.build dir)
        (Action.run
           (Ok path)
           [ flag_of_kind kind; Path.Build.basename input |> Filename.to_string ])
      |> Action.Full.make
    in
    (* Depend on [extra_deps] so if the ocamlformat config file
       changes then ocamlformat will run again. *)
    extra_deps dir >>> action >>| Action.Full.add_sandbox Sandbox_config.needs_sandboxing
  ;;

  let action_when_ocamlformat_isn't_locked ~input kind =
    let open Action_builder.O in
    let module S = String_with_vars in
    let+ () = Path.Build.parent_exn input |> extra_deps in
    Dune_lang.Action.chdir
      (S.make_pform Loc.none (Var Workspace_root))
      (Dune_lang.Action.run
         (S.make_text Loc.none (Pkg_dev_tool.exe_name Ocamlformat))
         [ S.make_text Loc.none (flag_of_kind kind)
         ; S.make_pform Loc.none (Var Input_file)
         ])
  ;;
end

let format_action format ~ocamlformat_is_locked ~input ~expander kind =
  let open Action_builder.O in
  match (format : Dialect.Format.t) with
  | Ocamlformat when ocamlformat_is_locked ->
    let+ build = Ocamlformat.action_when_ocamlformat_is_locked ~input kind
    and+ env = Action_builder.of_memo (Pkg_rules.dev_tool_env Ocamlformat) in
    Action.Full.add_env env build
  | _ ->
    assert (not ocamlformat_is_locked);
    let* expander = Action_builder.of_memo expander
    and* loc, action =
      match format with
      | Action (loc, action) -> Action_builder.return (loc, action)
      | Ocamlformat ->
        let+ action = Ocamlformat.action_when_ocamlformat_isn't_locked ~input kind in
        Loc.none, action
    in
    Pp_spec_rules.action_for_pp
      ~sandbox:Sandbox_config.default
      ~loc
      ~expander
      ~action
      ~src:input
;;

let setup_source_file
      sctx
      (config : Format_config.t)
      ~dialects
      ~ocamlformat_is_locked
      ~expander
      ~dir
      alias
      file
  =
  let action =
    let open Option.O in
    let* dialect, kind =
      Path.Source.extension file
      |> Filename.Extension.Or_empty.extension
      |> Option.bind ~f:(Dialect.DB.find_by_extension dialects)
    in
    let* () =
      Option.some_if (Format_config.includes config (Dialect (Dialect.name dialect))) ()
    in
    let+ format =
      match Dialect.format dialect kind with
      | Some _ as action -> action
      | None ->
        (match Dialect.preprocess dialect kind with
         | None -> Dialect.format Dialect.ocaml kind
         | Some _ -> None)
    in
    kind, format
  in
  match action with
  | None -> Action_builder.return ()
  | Some (kind, format) ->
    let loc = Loc.in_file (Path.source file) in
    let input = Path.Build.relative_fname dir (Path.Source.basename file) in
    format_action format ~ocamlformat_is_locked ~input ~expander kind
    |> formatter_diff_action sctx ~loc alias ~input
;;

let setup_dune_files =
  let setup_dune_file sctx ~version ~dir alias path =
    let input_build = Path.Build.relative_fname dir (Path.Source.basename path) in
    let build =
      let input = Path.build input_build in
      let open Action_builder.O in
      let+ () = Action_builder.path input in
      Action.Full.make (Format_dune_file.action_stdout ~version input)
    in
    formatter_diff_action
      sctx
      ~loc:(Loc.in_file (Path.source path))
      alias
      ~input:input_build
      build
  in
  fun sctx (config : Format_config.t) ~version ~dir alias source_dir ->
    match Format_config.includes config Dune with
    | false -> Action_builder.return ()
    | true ->
      (match Source_tree.Dir.dune_file source_dir with
       | None -> Action_builder.return ()
       | Some dune_file ->
         (match Source.Dune_file.path dune_file with
          | None -> Action_builder.return ()
          | Some path -> setup_dune_file sctx ~version ~dir alias path))
;;

let setup_source_files
      sctx
      config
      ~dialects
      ~ocamlformat_is_locked
      ~expander
      ~dir
      alias
      source_dir
  =
  Source_tree.Dir.filenames source_dir
  |> Filename.Array.Set.to_list
  |> List.map ~f:(fun file ->
    Path.Source.relative_fname (Source_tree.Dir.path source_dir) file
    |> setup_source_file sctx config ~dialects ~ocamlformat_is_locked ~expander ~dir alias)
  |> Action_builder.all_unit
;;

let gen_format_alias sctx (config : Format_config.t) ~version ~dialects ~expander ~dir =
  let open Action_builder.O in
  Action_builder.of_memo (Source_tree.find_dir (Path.Build.drop_build_context_exn dir))
  >>= function
  | None -> Action_builder.return ()
  | Some source_dir ->
    let* ocamlformat_is_locked =
      Action_builder.of_memo (Ocamlformat.dev_tool_lock_dir_exists ())
    in
    let alias = Alias.make Alias0.fmt ~dir in
    let+ () =
      setup_source_files
        sctx
        config
        ~dialects
        ~ocamlformat_is_locked
        ~expander
        ~dir
        alias
        source_dir
    and+ () = setup_dune_files sctx config ~version ~dir alias source_dir in
    ()
;;

let format_config ~dir =
  let+ value =
    Env_stanza_db.value_opt ~dir ~f:(fun (t : Dune_env.config) ->
      Memo.return t.format_config)
  and+ default =
    (* we always force the default for error checking *)
    Path.Build.drop_build_context_exn dir
    |> Source_tree.nearest_dir
    >>| Source_tree.Dir.project
    >>| Dune_project.format_config
  in
  Option.value value ~default
;;

let with_config ~dir f =
  let* config = format_config ~dir in
  if Format_config.is_empty config
  then
    (* CR-someday rgrinberg: this [is_empty] check is weird. We should use [None]
       to represent that no settings have been set. *)
    Memo.return ()
  else f config
;;

let setup_alias sctx ~dir =
  with_config ~dir (fun config ->
    (* Keep formatter discovery behind the alias dependency. Loading a directory
       should not enumerate format inputs unless @fmt is built. *)
    let format =
      let open Action_builder.O in
      let expander = Super_context.expander sctx ~dir in
      let* project = Action_builder.of_memo (Dune_load.find_project ~dir) in
      let dialects = Dune_project.dialects project in
      let version = Dune_project.dune_version project in
      gen_format_alias sctx config ~version ~dialects ~expander ~dir
    in
    Rules.Produce.Alias.add_deps (Alias.make Alias0.fmt ~dir) format)
;;
