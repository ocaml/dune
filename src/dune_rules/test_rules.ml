open Import

let rules (t : Tests.t) ~sctx ~dir ~scope ~expander ~dir_contents =
  let test_kind (loc, name, ext) =
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
  in
  let open Memo.O in
  let runtest_modes =
    if Dune_project.dune_version (Scope.project scope) < (3, 0)
    then [ `exe ]
    else
      Executables.Link_mode.Map.to_list t.exes.modes
      |> List.filter_map ~f:(fun ((mode : Executables.Link_mode.t), _) ->
        match mode with
        | Byte_complete -> Some `exe
        | Other { kind = Exe; mode = Native | Best } -> Some `exe
        | Other { kind = Exe; mode = Byte } -> Some `bc
        | Other { kind = Js; _ } -> Some `js
        | Other { kind = C | Object | Shared_object | Plugin; _ } ->
          (* We don't know how to run tests in these cases *)
          None)
      |> List.sort_uniq ~compare:Poly.compare
  in
  let* () =
    Nonempty_list.to_list t.exes.names
    |> Memo.parallel_iter ~f:(fun (loc, s) ->
      Memo.parallel_iter runtest_modes ~f:(fun runtest_mode ->
        let ext =
          match runtest_mode with
          | `js -> Js_of_ocaml.Ext.exe
          | `bc -> ".bc"
          | `exe -> ".exe"
        in
        let custom_runner =
          match runtest_mode with
          | `js -> Some Jsoo_rules.runner
          | `bc | `exe -> None
        in
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
          let test_exe_path =
            Expander.map_exe expander (Path.relative (Path.build dir) test_exe)
          in
          Pform.Map.singleton test_pform [ Value.Path test_exe_path ]
        in
        let* runtest_alias =
          match runtest_mode with
          | `js -> Jsoo_rules.js_of_ocaml_runtest_alias ~dir
          | `exe | `bc -> Memo.return Alias0.runtest
        in
        let deps =
          match custom_runner with
          | Some _ ->
            Bindings.Unnamed (Dep_conf.File (String_with_vars.make_text loc test_exe))
            :: t.deps
          | None -> t.deps
        in
        let add_alias ~loc ~action =
          (* CR rgrinberg: why are we going through the stanza api? *)
          let alias =
            { Alias_conf.name = runtest_alias
            ; locks = t.locks
            ; package = t.package
            ; deps
            ; action = Some (loc, action)
            ; enabled_if = t.enabled_if
            ; loc
            }
          in
          Simple_rules.alias sctx ~extra_bindings ~dir ~expander alias
        in
        match test_kind (loc, s, ext) with
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
                Simple_rules.user_rule sctx rule ~extra_bindings ~dir ~expander
              in
              ()))
  in
  Exe_rules.rules t.exes ~sctx ~dir ~scope ~expander ~dir_contents
;;
