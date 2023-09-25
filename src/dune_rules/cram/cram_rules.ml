open Import

type effective =
  { loc : Loc.t
  ; alias : Alias.Name.Set.t
  ; deps : unit Action_builder.t list
  ; shell : (Path.t * string list) Action_builder.t option
  ; sandbox : Sandbox_config.t
  ; enabled_if : Blang.t list
  ; locks : Path.Set.t
  ; packages : Package.Name.Set.t
  }

let empty_effective =
  { loc = Loc.none
  ; alias = Alias.Name.Set.singleton Alias0.runtest
  ; enabled_if = [ Blang.true_ ]
  ; locks = Path.Set.empty
  ; deps = []
  ; shell = None
  ; sandbox = Sandbox_config.needs_sandboxing
  ; packages = Package.Name.Set.empty
  }
;;

let missing_run_t (error : Cram_test.t) =
  Action_builder.fail
    { fail =
        (fun () ->
          let dir =
            match error with
            | File _ ->
              (* This error is impossible for file tests *)
              assert false
            | Dir { dir; file = _ } -> dir
          in
          User_error.raise
            [ Pp.textf
                "Cram test directory %s does not contain a run.t file."
                (Path.Source.to_string dir)
            ])
    }
;;

let test_rule
  ~sctx
  ~expander
  ~dir
  (spec : effective)
  (test : (Cram_test.t, Source_tree.Dir.error) result)
  =
  let open Memo.O in
  let module Alias_rules = Simple_rules.Alias_rules in
  let* enabled = Expander.eval_blang expander (Blang.And spec.enabled_if) in
  let loc = spec.loc in
  let aliases = Alias.Name.Set.to_list_map spec.alias ~f:(Alias.make ~dir) in
  match test with
  | Error (Missing_run_t test) ->
    (* We error out on invalid tests even if they are disabled. *)
    Memo.parallel_iter aliases ~f:(fun alias ->
      Alias_rules.add sctx ~alias ~loc (missing_run_t test))
  | Ok test ->
    (match enabled with
     | false ->
       Memo.parallel_iter aliases ~f:(fun alias -> Alias_rules.add_empty sctx ~alias ~loc)
     | true ->
       let prefix_with, _ = Path.Build.extract_build_context_dir_exn dir in
       let script = Path.Build.append_source prefix_with (Cram_test.script test) in
       let action ~shell:(shell_prog, shell_args) =
         Action.progn
           [ Cram_exec.action { script = Path.build script; shell_prog; shell_args }
           ; Diff
               { Diff.optional = true
               ; mode = Text
               ; file1 = Path.build script
               ; file2 = Path.Build.extend_basename script ~suffix:".corrected"
               }
           ]
       in
       let locks = Path.Set.to_list spec.locks in
       let cram =
         let open Action_builder.O in
         let+ () = Action_builder.path (Path.build script)
         and+ () = Action_builder.all_unit spec.deps
         and+ (_ : Path.Set.t) =
           match test with
           | File _ -> Action_builder.return Path.Set.empty
           | Dir { dir; file = _ } ->
             let deps =
               Path.Build.append_source prefix_with dir |> Path.build |> Source_deps.files
             in
             Action_builder.dyn_memo_deps deps
         and+ shell =
           match spec.shell with
           | None ->
             let context = Expander.context expander in
             Action_builder.return
               (Cram_stanza.system_shell_prog ~loc ~context `sh |> Action.Prog.ok_exn, [])
           | Some p -> p
         in
         Action.Full.make (action ~shell) ~locks ~sandbox:spec.sandbox
       in
       Memo.parallel_iter aliases ~f:(fun alias -> Alias_rules.add sctx ~alias ~loc cram))
;;

let rules ~sctx ~expander ~dir tests =
  let open Memo.O in
  let* stanzas =
    let stanzas dir ~f =
      let+ stanzas = Only_packages.stanzas_in_dir dir in
      match stanzas with
      | None -> []
      | Some (d : Dune_file.t) ->
        List.filter_map d.stanzas ~f:(function
          | Cram_stanza.T c -> Option.some_if (f c) (dir, c)
          | _ -> None)
    in
    let rec collect_whole_subtree acc dir =
      let* acc =
        let+ cram =
          stanzas dir ~f:(fun (s : Cram_stanza.t) -> s.applies_to = Whole_subtree)
        in
        cram :: acc
      in
      match Path.Build.parent dir with
      | None -> Memo.return (List.concat acc)
      | Some dir -> collect_whole_subtree acc dir
    in
    let* acc = stanzas dir ~f:(fun _ -> true) in
    match Path.Build.parent dir with
    | None -> Memo.return acc
    | Some dir -> collect_whole_subtree [ acc ] dir
  in
  Memo.parallel_iter tests ~f:(fun test ->
    let name =
      match test with
      | Ok test -> Cram_test.name test
      | Error (Source_tree.Dir.Missing_run_t test) -> Cram_test.name test
    in
    let open Memo.O in
    let* effective =
      let init =
        let alias =
          Alias.Name.of_string name |> Alias.Name.Set.add empty_effective.alias
        in
        Memo.return { empty_effective with alias }
      in
      List.fold_left stanzas ~init ~f:(fun acc (dir, (spec : Cram_stanza.t)) ->
        match
          match spec.applies_to with
          | Whole_subtree -> true
          | Files_matching_in_this_dir pred ->
            Predicate_lang.Glob.test pred ~standard:Predicate_lang.true_ name
        with
        | false -> acc
        | true ->
          let* acc = acc in
          let* expander = Super_context.expander sctx ~dir in
          let shell, deps, sandbox =
            let return ?shell_deps ?sandbox shell =
              let sandbox =
                match sandbox with
                | None -> acc.sandbox
                | Some s -> Sandbox_config.inter acc.sandbox s
              and deps =
                match shell_deps with
                | None -> acc.deps
                | Some dep -> dep :: acc.deps
              in
              shell, deps, sandbox
            in
            match spec.shell with
            | System_shell -> return None
            | Custom_shell { prog; args } ->
              let shell_deps, sandbox = Dep_conf_eval.unnamed ~expander [ File prog ] in
              let dir = Path.build dir in
              let expand_arg sw =
                let open Action_builder.O in
                let+ v =
                  String_expander.Action_builder.expand
                    sw
                    ~dir
                    ~mode:Many
                    ~f:(Expander.expand_pform expander)
                in
                Value.L.to_strings v ~dir
              in
              let shell =
                let open Action_builder.O in
                let+ shell_args = Action_builder.all (List.map args ~f:expand_arg)
                and+ shell_prog =
                  Expander.(
                    With_deps_if_necessary.expand_single_path expander prog
                    |> Deps.action_builder)
                in
                shell_prog, List.concat shell_args
              in
              return ~shell_deps ~sandbox (Some shell)
          in
          let deps, sandbox =
            match spec.deps with
            | None -> deps, sandbox
            | Some deps' ->
              let (deps' : unit Action_builder.t), _, sandbox' =
                Dep_conf_eval.named ~expander deps'
              in
              deps' :: deps, Sandbox_config.inter sandbox sandbox'
          in
          let enabled_if = spec.enabled_if :: acc.enabled_if in
          let alias =
            match spec.alias with
            | None -> acc.alias
            | Some a -> Alias.Name.Set.add acc.alias a
          in
          let packages =
            match spec.package with
            | None -> acc.packages
            | Some (p : Package.t) ->
              Package.Name.Set.add acc.packages (Package.Id.name p.id)
          in
          let+ locks =
            (* Locks must be relative to the cram stanza directory and not
               the individual tests directories *)
            let base = `This (Path.build dir) in
            Expander.expand_locks ~base expander spec.locks
            >>| Path.Set.of_list
            >>| Path.Set.union acc.locks
          in
          { acc with enabled_if; locks; deps; shell; alias; packages; sandbox })
    in
    let test_rule () = test_rule ~sctx ~expander ~dir effective test in
    Only_packages.get_mask ()
    >>= function
    | None -> test_rule ()
    | Some only ->
      let only = Package.Name.Map.keys only |> Package.Name.Set.of_list in
      Memo.when_
        (Package.Name.Set.is_empty effective.packages
         || Package.Name.Set.(not (is_empty (inter only effective.packages))))
        test_rule)
;;
