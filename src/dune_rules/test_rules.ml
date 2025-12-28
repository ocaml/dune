open Import
open Memo.O

let runtest_alias mode ~dir =
  (match mode with
   | `js mode -> Jsoo_rules.js_of_ocaml_runtest_alias ~dir ~mode
   | `exe | `bc -> Memo.return Alias0.runtest)
  >>| Alias.make ~dir
;;

let test_kind dir_contents (loc, name, ext) =
  let files = Dir_contents.text_files dir_contents in
  let expected_basename = name ^ ".expected" in
  if Filename.Set.mem files expected_basename
  then
    `Expect
      { Diff.file1 = String_with_vars.make_text loc expected_basename
      ; file2 = String_with_vars.make_text loc (name ^ ext ^ ".output")
      ; optional = false
      ; mode = Text
      }
  else `Regular
;;

let ext_of_mode runtest_mode =
  match runtest_mode with
  | `js mode -> Js_of_ocaml.Ext.exe ~mode
  | `bc -> ".bc"
  | `exe -> ".exe"
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
    let* runtest_modes =
      let+ jsoo_enabled_modes =
        Jsoo_rules.jsoo_enabled_modes
          ~expander
          ~dir
          ~in_context:(Js_of_ocaml.In_context.make ~dir t.exes.buildable.js_of_ocaml)
      in
      runtest_modes t.exes.modes jsoo_enabled_modes (Scope.project scope)
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
          let test_exe = s ^ ext in
          let extra_bindings =
            let test_exe_path, _, _ =
              Expander.map_exe expander (Path.relative (Path.build dir) test_exe) []
            in
            Pform.Map.singleton test_pform [ Value.Path test_exe_path ]
          in
          let* runtest_alias = runtest_alias runtest_mode ~dir in
          let deps =
            (* is this useless? we are going to infer the dependency anyway *)
            match custom_runner with
            | None -> t.deps
            | Some _ ->
              Bindings.Unnamed (Dep_conf.File (String_with_vars.make_text loc test_exe))
              ::
              (match runtest_mode with
               | `js Wasm ->
                 Bindings.Unnamed
                   (Dep_conf.File
                      (String_with_vars.make_text loc (s ^ Js_of_ocaml.Ext.wasm_dir)))
                 :: t.deps
               | `js JS | `exe | `bc -> t.deps)
          in
          let add_alias =
            let expander = Expander.add_bindings expander ~bindings:extra_bindings in
            let alias =
              [ Alias.Name.to_string (Alias.name runtest_alias); s ]
              |> String.concat ~sep:"-"
              |> Alias.Name.of_string
              |> Alias.make ~dir
            in
            fun ~loc ~action ->
              let action =
                let chdir = Expander.dir expander in
                Action_unexpanded.expand_no_targets
                  action
                  ~loc
                  ~expander
                  ~chdir
                  ~deps
                  ~what:"aliases"
              in
              Simple_rules.interpret_and_add_locks ~expander t.locks action
              |> Simple_rules.Alias_rules.add sctx ~loc ~alias
              >>> (Dep.alias alias
                   |> Action_builder.dep
                   |> Rules.Produce.Alias.add_deps runtest_alias)
          in
          match test_kind dir_contents (loc, s, ext) with
          | `Regular -> add_alias ~loc ~action:run_action
          | `Expect diff ->
            let rule =
              { Rule_conf.targets = Infer
              ; deps
              ; action =
                  ( loc
                  , Action_unexpanded.Redirect_out (Stdout, diff.file2, Normal, run_action)
                  )
              ; mode = Standard
              ; locks = t.locks
              ; loc
              ; enabled_if = t.enabled_if
              ; aliases = []
              ; package = t.package
              }
            in
            add_alias ~loc ~action:(Diff diff)
            >>> let+ (_ignored_targets : Targets.Validated.t option) =
                  (* CR-someday rgrinberg: use direct api *)
                  Simple_rules.user_rule sctx rule ~extra_bindings ~dir ~expander
                in
                ()))
  in
  Exe_rules.rules t.exes ~sctx ~scope ~expander ~dir_contents
;;
