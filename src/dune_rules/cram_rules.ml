open Import

type effective =
  { loc : Loc.t
  ; alias : Alias.Name.Set.t
  ; deps : unit Action_builder.t list
  ; sandbox : Sandbox_config.t
  ; enabled_if : Blang.t list
  ; locks : Path.Set.t
  ; packages : Package.Name.Set.t
  }

let empty_effective =
  { loc = Loc.none
  ; alias = Alias.Name.Set.singleton Alias.Name.runtest
  ; enabled_if = [ Blang.true_ ]
  ; locks = Path.Set.empty
  ; deps = []
  ; sandbox = Sandbox_config.needs_sandboxing
  ; packages = Package.Name.Set.empty
  }

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
            [ Pp.textf "Cram test directory %s does not contain a run.t file."
                (Path.Source.to_string dir)
            ])
    }

let test_rule ~sctx ~expander ~dir (spec : effective)
    (test : (Cram_test.t, Source_tree.Dir.error) result) =
  let open Memo.O in
  let module Alias_rules = Simple_rules.Alias_rules in
  let* enabled = Expander.eval_blang expander (Blang.And spec.enabled_if) in
  let loc = Some spec.loc in
  let aliases = Alias.Name.Set.to_list_map spec.alias ~f:(Alias.make ~dir) in
  match test with
  | Error (Missing_run_t test) ->
    (* We error out on invalid tests even if they are disabled. *)
    Memo.parallel_iter aliases ~f:(fun alias ->
        Alias_rules.add sctx ~alias ~loc (missing_run_t test))
  | Ok test -> (
    match enabled with
    | false ->
      Memo.parallel_iter aliases ~f:(fun alias ->
          Alias_rules.add_empty sctx ~alias ~loc)
    | true ->
      let prefix_with, _ = Path.Build.extract_build_context_dir_exn dir in
      let script =
        Path.Build.append_source prefix_with (Cram_test.script test)
      in
      let action =
        Action.progn
          [ Cram_exec.action (Path.build script)
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
            let dir = Path.build (Path.Build.append_source prefix_with dir) in
            Action_builder.source_tree ~dir
        in
        Action.Full.make action ~locks ~sandbox:spec.sandbox
      in
      Memo.parallel_iter aliases ~f:(fun alias ->
          Alias_rules.add sctx ~alias ~loc cram))

let rules ~sctx ~expander ~dir tests =
  let open Memo.O in
  let* stanzas =
    let stanzas dir ~f =
      let+ stanzas = Only_packages.stanzas_in_dir dir in
      match stanzas with
      | None -> []
      | Some (d : Dune_file.t) ->
        List.filter_map d.stanzas ~f:(function
          | Dune_file.Cram c -> Option.some_if (f c) (dir, c)
          | _ -> None)
    in
    let rec collect_whole_subtree acc dir =
      let* acc =
        let+ cram =
          stanzas dir ~f:(fun (s : Cram_stanza.t) ->
              s.applies_to = Whole_subtree)
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
            Alias.Name.of_string name
            |> Alias.Name.Set.add empty_effective.alias
          in
          Memo.return { empty_effective with alias }
        in
        List.fold_left stanzas ~init
          ~f:(fun acc (dir, (spec : Cram_stanza.t)) ->
            match
              match spec.applies_to with
              | Whole_subtree -> true
              | Files_matching_in_this_dir pred ->
                Predicate_lang.Glob.exec pred
                  ~standard:Predicate_lang.Glob.true_ name
            with
            | false -> acc
            | true ->
              let* acc = acc in
              let* deps, sandbox =
                match spec.deps with
                | None -> Memo.return (acc.deps, acc.sandbox)
                | Some deps ->
                  let+ (deps : unit Action_builder.t), _, sandbox =
                    let+ expander = Super_context.expander sctx ~dir in
                    Dep_conf_eval.named ~expander deps
                  in
                  (deps :: acc.deps, Sandbox_config.inter acc.sandbox sandbox)
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
                >>| Path.Set.of_list >>| Path.Set.union acc.locks
              in
              { acc with enabled_if; locks; deps; alias; packages; sandbox })
      in
      let test_rule () = test_rule ~sctx ~expander ~dir effective test in
      Only_packages.get_mask () >>= function
      | None -> test_rule ()
      | Some only ->
        let only = Package.Name.Map.keys only |> Package.Name.Set.of_list in
        Memo.when_
          (Package.Name.Set.is_empty effective.packages
          || Package.Name.Set.(not (is_empty (inter only effective.packages))))
          test_rule)
