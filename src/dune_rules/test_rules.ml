open Import
open Memo.O

let runtest_alias mode ~dir =
  (match mode with
   | `js mode -> Jsoo_rules.js_of_ocaml_runtest_alias ~dir ~mode
   | `exe | `bc -> Memo.return Alias0.runtest)
  >>| Alias.make ~dir
;;

let test_kind ~dir dir_contents name ext =
  (* let dir = Dir_contents.dir dir_contents in *)
  let files = Dir_contents.text_files dir_contents in
  let expected_basename = name ^ ".expected" in
  if Filename.Set.mem files expected_basename
  then
    `Expect
      { Diff.file1 = Path.build (Path.Build.relative dir expected_basename)
      ; file2 =
          Path.Build.relative dir (name ^ Filename.Extension.to_string ext ^ ".output")
      ; optional = false
      ; mode = Text
      }
  else `Regular
;;

let ext_of_mode runtest_mode =
  match runtest_mode with
  | `js mode -> Js_of_ocaml.Ext.exe ~mode
  | `bc -> Filename.Extension.bc
  | `exe -> Filename.Extension.exe
;;

let custom_runner runtest_mode =
  match runtest_mode with
  | `js _ -> Some Jsoo_rules.runner
  | `bc | `exe -> None
;;

let runtest_modes modes jsoo_enabled_modes project =
  if Dune_project.dune_version project < (3, 0)
  then [ `exe ]
  else
    Executables.Link_mode.Map.to_list modes
    |> List.filter_map ~f:(fun ((mode : Executables.Link_mode.t), _) ->
      match mode with
      | Byte_complete -> Some `exe
      | Other { kind = Exe; mode = Native | Best } -> Some `exe
      | Other { kind = Exe; mode = Byte } -> Some `bc
      | Other { kind = C | Object | Shared_object | Plugin; _ } ->
        (* We don't know how to run tests in these cases *)
        None
      | Jsoo mode ->
        Option.some_if (Js_of_ocaml.Mode.Pair.select ~mode jsoo_enabled_modes) (`js mode))
    |> List.sort_uniq ~compare:Poly.compare
;;

let rules (t : Tests.t) ~sctx ~dir ~scope ~expander ~dir_contents =
  let* () =
    let project = Scope.project scope in
    let* runtest_modes =
      let+ jsoo_enabled_modes =
        Jsoo_rules.jsoo_enabled_modes
          ~expander
          ~dir
          ~in_context:(Js_of_ocaml.In_context.make ~dir t.exes.buildable.js_of_ocaml)
      in
      runtest_modes t.exes.modes jsoo_enabled_modes project
    in
    Expander.eval_blang expander t.enabled_if
    >>= function
    | false ->
      let loc = Nonempty_list.hd t.exes.names |> fst in
      Memo.parallel_iter runtest_modes ~f:(fun mode ->
        let* alias = runtest_alias mode ~dir in
        Simple_rules.Alias_rules.add_empty sctx ~loc ~alias)
    | true ->
      Nonempty_list.to_list t.exes.names
      |> Memo.parallel_iter ~f:(fun (loc, s) ->
        Memo.parallel_iter runtest_modes ~f:(fun runtest_mode ->
          let ext = ext_of_mode runtest_mode in
          let custom_runner = custom_runner runtest_mode in
          let test_pform = Pform.Var Test in
          let run_action =
            match t.action with
            | Some a -> a
            | None ->
              (match custom_runner with
               | None ->
                 Action_unexpanded.run (String_with_vars.make_pform loc test_pform) []
               | Some runner ->
                 Action_unexpanded.run
                   (String_with_vars.make_text loc runner)
                   [ String_with_vars.make_pform loc test_pform ])
          in
          let test_exe = s ^ Filename.Extension.to_string ext in
          let extra_bindings =
            let test_exe_path, _, _ =
              Expander.map_exe
                ~force_host:false
                expander
                (Path.relative (Path.build dir) test_exe)
                []
            in
            Pform.Map.singleton test_pform [ Value.Path test_exe_path ]
          in
          let* runtest_alias = runtest_alias runtest_mode ~dir in
          let deps =
            match custom_runner with
            | None -> t.deps
            | Some _ ->
              Bindings.Unnamed (Dep_conf.File (String_with_vars.make_text loc test_exe))
              ::
              (match runtest_mode with
               | `js Wasm ->
                 Bindings.Unnamed
                   (Dep_conf.File
                      (String_with_vars.make_text
                         loc
                         (s ^ Filename.Extension.to_string Js_of_ocaml.Ext.wasm_dir)))
                 :: t.deps
               | `js JS | `exe | `bc -> t.deps)
          in
          let add_alias =
            let alias =
              [ Alias.Name.to_string (Alias.name runtest_alias); s ]
              |> String.concat ~sep:"-"
              |> Alias.Name.of_string
              |> Alias.make ~dir
            in
            fun ~loc action ->
              Simple_rules.Alias_rules.add sctx ~loc ~alias action
              >>> (Dep.alias alias
                   |> Action_builder.dep
                   |> Rules.Produce.Alias.add_deps runtest_alias)
          in
          let expander = Expander.add_bindings expander ~bindings:extra_bindings in
          let sandbox =
            if Dune_project.dune_version project >= (3, 22)
            then Sandbox_config.needs_sandboxing
            else Sandbox_config.no_special_requirements
          in
          match test_kind ~dir:(Expander.dir expander) dir_contents s ext with
          | `Regular ->
            let action =
              let chdir = Expander.dir expander in
              Action_unexpanded.expand_no_targets
                run_action
                sandbox
                ~loc
                ~expander
                ~chdir
                ~deps
                ~what:"aliases"
            in
            Simple_rules.interpret_and_add_locks ~expander t.locks action
            |> add_alias ~loc
          | `Expect diff ->
            let* (_ignored_targets : Targets.Validated.t) =
              let* mode =
                Rule_mode_expand.expand_path ~expander ~dir Rule_mode.Standard
              in
              (let+ action =
                 Action_unexpanded.expand
                   ~chdir:(Expander.dir expander)
                   ~loc
                   ~expander
                   ~deps
                   ~targets:Infer
                   ~targets_dir:dir
                   run_action
                   sandbox
               in
               Action_builder.With_targets.add action ~file_targets:[ diff.file2 ]
               |> Action_builder.With_targets.map_build ~f:(fun action ->
                 Action_builder.map
                   action
                   ~f:(Action.Full.map ~f:(Action.with_stdout_to diff.file2))
                 |> Simple_rules.interpret_and_add_locks ~expander t.locks))
              >>= Super_context.add_rule_get_targets sctx ~dir ~mode ~loc
            in
            add_alias
              ~loc
              (let open Action_builder.O in
               let+ () = Action_builder.paths [ diff.file1; Path.build diff.file2 ] in
               Action.Full.make (Action.Diff diff))))
  in
  Exe_rules.rules t.exes ~sctx ~scope ~expander ~dir_contents
;;
