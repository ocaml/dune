open Import
open Memo.O

module Spec = struct
  type t =
    { loc : Loc.t
    ; test_name_alias : Alias.Name.t
    ; extra_aliases : Alias.Name.Set.t
    ; deps : unit Action_builder.t list
    ; sandbox : Sandbox_config.t
    ; enabled_if : (Expander.t * Blang.t) list
    ; locks : Path.Set.t Action_builder.t
    ; packages : Package.Name.Set.t
    ; timeout : (Loc.t * float) option
    ; conflict_markers : Cram_stanza.Conflict_markers.t
    ; setup_scripts : Path.t list
    }

  let make_empty ~test_name_alias =
    { loc = Loc.none
    ; test_name_alias
    ; extra_aliases = Alias.Name.Set.empty
    ; enabled_if = []
    ; locks = Action_builder.return Path.Set.empty
    ; deps = []
    ; sandbox = Sandbox_config.needs_sandboxing
    ; packages = Package.Name.Set.empty
    ; timeout = None
    ; conflict_markers = Ignore
    ; setup_scripts = []
    }
  ;;
end

type error = Missing_run_t of Cram_test.t

let missing_run_t (error : Cram_test.t) =
  let dir =
    match error with
    | File _ ->
      (* This error is impossible for file tests *)
      assert false
    | Dir { dir; file = _ } -> dir
  in
  User_error.raise
    ~loc:(Loc.in_dir (Path.source dir))
    [ Pp.textf
        "Cram test directory %s does not contain a run.t file."
        (Path.Source.to_string dir)
    ]
;;

let test_rule
      ~sctx
      ~dir
      ({ test_name_alias
       ; extra_aliases
       ; loc
       ; enabled_if
       ; deps
       ; locks
       ; sandbox
       ; packages = _
       ; timeout
       ; conflict_markers
       ; setup_scripts
       } :
        Spec.t)
      (test : (Cram_test.t, error) result)
  =
  let module Alias_rules = Simple_rules.Alias_rules in
  let extra_aliases =
    Alias.Name.Set.remove extra_aliases test_name_alias
    |> Alias.Name.Set.to_list_map ~f:(Alias.make ~dir)
  in
  let alias = Alias.make ~dir test_name_alias in
  (* Here we get all other aliases to depend on the main alias which is the same as the
     name of the cram test. *)
  let* () =
    Memo.parallel_iter extra_aliases ~f:(fun extra_alias ->
      Rules.Produce.Alias.add_deps ~loc extra_alias (Action_builder.dep (Dep.alias alias)))
  in
  match test with
  | Error (Missing_run_t test) ->
    (* We error out on invalid tests even if they are disabled. *)
    Action_builder.fail { fail = (fun () -> missing_run_t test) }
    |> Alias_rules.add sctx ~alias ~loc
  | Ok test ->
    (* Morally, this is equivalent to evaluating them all concurrently and
       taking the conjunction, but we do it this way to avoid evaluating things
       unnecessarily *)
    Memo.List.for_all enabled_if ~f:(fun (expander, blang) ->
      Expander.eval_blang expander blang)
    >>= (function
     | false -> Alias_rules.add_empty sctx ~alias ~loc
     | true ->
       let prefix_with, _ = Path.Build.extract_build_context_dir_exn dir in
       let script = Path.Build.append_source prefix_with (Cram_test.script test) in
       let base_path =
         Path.Build.append_source
           prefix_with
           (let path =
              match test with
              | File f -> f
              | Dir d -> d.dir
            in
            let dir = Path.Source.parent_exn path in
            let basename = Path.Source.basename path in
            Path.Source.relative dir (".cram." ^ basename))
       in
       let script_sh = Path.Build.relative base_path "cram.sh" in
       let output = Path.Build.relative base_path "cram.out" in
       let* () =
         (let open Action_builder.O in
          let+ () = Action_builder.path (Path.build script) in
          Cram_exec.make_script
            ~src:(Path.build script)
            ~script:script_sh
            ~conflict_markers
          |> Action.Full.make)
         |> Action_builder.with_file_targets ~file_targets:[ script_sh ]
         |> Super_context.add_rule sctx ~dir ~loc
       in
       let* () =
         (let open Action_builder.O in
          let+ () = Action_builder.all_unit deps
          and+ () = Action_builder.path (Path.build script_sh)
          and+ () =
            match test with
            | File _ -> Action_builder.return ()
            | Dir { dir; file } ->
              let file = Path.Build.append_source prefix_with file |> Path.build in
              let deps =
                Path.Build.append_source prefix_with dir
                |> Path.build
                |> Source_deps.files_with_filter ~filter:(fun file' ->
                  not (Path.equal file file'))
              in
              let+ (_ : Path.Set.t) = Action_builder.dyn_memo_deps deps in
              ()
          and+ () = Action_builder.paths setup_scripts
          and+ locks = locks >>| Path.Set.to_list in
          Cram_exec.run
            ~src:(Path.build script)
            ~dir:
              (Path.build
                 (match test with
                  | File _ -> Path.Build.parent_exn script
                  | Dir d -> Path.Build.append_source prefix_with d.dir))
            ~script:(Path.build script_sh)
            ~output
            ~timeout
            ~setup_scripts
          |> Action.Full.make ~locks ~sandbox)
         |> Action_builder.with_file_targets ~file_targets:[ output ]
         |> Super_context.add_rule sctx ~dir ~loc
       in
       Alias_rules.add sctx ~alias ~loc
       @@
       let open Action_builder.O in
       let+ () = List.map ~f:Path.build [ script; output ] |> Action_builder.paths in
       Action.progn
         [ Cram_exec.diff ~src:(Path.build script) ~output:(Path.build output)
         ; Promote.Diff_action.diff
             ~optional:true
             ~mode:Text
             (Path.build script)
             (Path.Build.extend_basename script ~suffix:".corrected")
         ]
       |> Action.Full.make)
;;

let collect_stanzas =
  let stanzas dir ~f =
    Dune_load.stanzas_in_dir dir
    >>= function
    | None -> Memo.return []
    | Some (d : Dune_file.t) ->
      Dune_file.find_stanzas d Cram_stanza.key
      >>| List.filter_map ~f:(fun c -> Option.some_if (f c) (dir, c))
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
  fun ~dir ->
    let* acc = stanzas dir ~f:(fun _ -> true) in
    match Path.Build.parent dir with
    | None -> Memo.return acc
    | Some dir -> collect_whole_subtree [ acc ] dir
;;

let rules ~sctx ~dir tests =
  let* stanzas = collect_stanzas ~dir
  and* with_package_mask =
    Dune_load.mask ()
    >>| function
    | None -> fun _packages f -> f ()
    | Some only ->
      let only = Package.Name.Set.of_keys only in
      fun packages f ->
        Memo.when_
          (Package.Name.Set.is_empty packages
           || Package.Name.Set.(not (is_empty (inter only packages))))
          f
  in
  Memo.parallel_iter tests ~f:(fun test ->
    let* spec =
      let name =
        match test with
        | Ok test -> Cram_test.name test
        | Error (Missing_run_t test) -> Cram_test.name test
      in
      let test_name_alias = Alias.Name.of_string name in
      let init = None, Spec.make_empty ~test_name_alias in
      let+ runtest_alias, acc =
        Memo.List.fold_left
          stanzas
          ~init
          ~f:(fun (runtest_alias, (acc : Spec.t)) (dir, (stanza : Cram_stanza.t)) ->
            match
              match stanza.applies_to with
              | Whole_subtree -> true
              | Files_matching_in_this_dir pred ->
                Predicate_lang.Glob.test pred ~standard:Predicate_lang.true_ name
            with
            | false -> Memo.return (runtest_alias, acc)
            | true ->
              let+ expander = Super_context.expander sctx ~dir in
              let deps, sandbox =
                match stanza.deps with
                | None -> acc.deps, acc.sandbox
                | Some deps ->
                  let (deps : unit Action_builder.t), _, sandbox =
                    Dep_conf_eval.named ~expander deps
                  in
                  deps :: acc.deps, Sandbox_config.inter acc.sandbox sandbox
              in
              let locks =
                let open Action_builder.O in
                let+ more_locks =
                  Expander.expand_locks expander stanza.locks >>| Path.Set.of_list
                and+ locks = acc.locks in
                Path.Set.union locks more_locks
              in
              let runtest_alias =
                match stanza.runtest_alias with
                | None -> None
                | Some (loc, set) ->
                  (match runtest_alias with
                   | None -> Some (loc, set)
                   | Some (loc', _) ->
                     let main_message =
                       [ Pp.text
                           "enabling or disabling the runtest alias for a cram test may \
                            only be set once."
                       ; Pp.textf "It's already set for the test %S" name
                       ]
                     in
                     let annots =
                       let main = User_message.make ~loc:loc' main_message in
                       let related =
                         [ User_message.make ~loc [ Pp.text "Already set here" ] ]
                       in
                       User_message.Annots.singleton
                         Compound_user_error.annot
                         [ Compound_user_error.make ~main ~related ]
                     in
                     User_error.raise
                       ~annots
                       ~loc
                       (main_message
                        @ [ Pp.text "The first definition is at:"
                          ; Pp.text (Loc.to_file_colon_line loc')
                          ]))
              in
              let enabled_if = (expander, stanza.enabled_if) :: acc.enabled_if in
              let extra_aliases =
                match stanza.alias with
                | None -> acc.extra_aliases
                | Some a -> Alias.Name.Set.add acc.extra_aliases a
              in
              let packages =
                match stanza.package with
                | None -> acc.packages
                | Some (p : Package.t) ->
                  Package.Name.Set.add acc.packages (Package.name p)
              in
              let timeout =
                Option.merge
                  acc.timeout
                  stanza.timeout
                  ~f:(Ordering.min (fun x y -> Float.compare (snd x) (snd y)))
              in
              let conflict_markers =
                Option.value ~default:acc.conflict_markers stanza.conflict_markers
              in
              let setup_scripts =
                let more_current_scripts =
                  List.map stanza.setup_scripts ~f:(fun (_loc, script) ->
                    (* Handle both relative and absolute paths *)
                    if Filename.is_relative script
                    then Path.build (Path.Build.relative dir script)
                    else Path.external_ (Path.External.of_string script))
                in
                (* This is a silly way to dedupe, but we aim to preserve the
                   order as much as possible. *)
                more_current_scripts
                @ List.filter acc.setup_scripts ~f:(fun x ->
                  not (List.mem more_current_scripts x ~equal:Path.equal))
              in
              ( runtest_alias
              , { acc with
                  enabled_if
                ; locks
                ; deps
                ; test_name_alias
                ; extra_aliases
                ; packages
                ; sandbox
                ; timeout
                ; conflict_markers
                ; setup_scripts
                } ))
      in
      let extra_aliases =
        let to_add =
          match runtest_alias with
          | None | Some (_, true) -> Alias.Name.Set.singleton Alias0.runtest
          | Some (_, false) -> Alias.Name.Set.empty
        in
        Alias.Name.Set.union to_add acc.extra_aliases
      in
      { acc with extra_aliases }
    in
    with_package_mask spec.packages (fun () -> test_rule ~sctx ~dir spec test))
;;

let cram_tests dir =
  match Dune_project.cram (Source_tree.Dir.project dir) with
  | false -> Memo.return []
  | true ->
    let path = Source_tree.Dir.path dir in
    let file_tests =
      Source_tree.Dir.filenames dir
      |> Filename.Set.to_list
      |> List.filter_map ~f:(fun s ->
        if Cram_test.is_cram_suffix s
        then Some (Ok (Cram_test.File (Path.Source.relative path s)))
        else None)
    in
    let+ dir_tests =
      Source_tree.Dir.sub_dirs dir
      |> Filename.Map.to_list
      |> Memo.parallel_map ~f:(fun (name, sub_dir) ->
        match Cram_test.is_cram_suffix name with
        | false -> Memo.return None
        | true ->
          let+ sub_dir = Source_tree.Dir.sub_dir_as_t sub_dir in
          let fname = Cram_test.fname_in_dir_test in
          let test =
            let dir = Source_tree.Dir.path sub_dir in
            let file = Path.Source.relative dir fname in
            Cram_test.Dir { file; dir }
          in
          let files = Source_tree.Dir.filenames sub_dir in
          if Filename.Set.is_empty files
          then None
          else
            Some
              (if Filename.Set.mem files fname
               then Ok test
               else Error (Missing_run_t test)))
      >>| List.filter_opt
    in
    file_tests @ dir_tests
;;

let rules ~sctx ~dir source_dir =
  cram_tests source_dir
  >>= function
  | [] -> Memo.return ()
  | tests -> rules ~sctx ~dir tests
;;
