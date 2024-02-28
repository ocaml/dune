open Import
open Memo.O

module Spec = struct
  type t =
    { loc : Loc.t
    ; alias : Alias.Name.Set.t
    ; deps : unit Action_builder.t list
    ; sandbox : Sandbox_config.t
    ; enabled_if : (Expander.t * Blang.t) list
    ; locks : Path.Set.t Action_builder.t
    ; packages : Package.Name.Set.t
    }

  let empty =
    { loc = Loc.none
    ; alias = Alias.Name.Set.empty
    ; enabled_if = []
    ; locks = Action_builder.return Path.Set.empty
    ; deps = []
    ; sandbox = Sandbox_config.needs_sandboxing
    ; packages = Package.Name.Set.empty
    }
  ;;
end

type error = Missing_run_t of Cram_test.t

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
            ~loc:(Loc.in_dir (Path.source dir))
            [ Pp.textf
                "Cram test directory %s does not contain a run.t file."
                (Path.Source.to_string dir)
            ])
    }
;;

let test_rule
  ~sctx
  ~dir
  ({ alias; loc; enabled_if; deps; locks; sandbox; packages = _ } : Spec.t)
  (test : (Cram_test.t, error) result)
  =
  let module Alias_rules = Simple_rules.Alias_rules in
  let aliases = Alias.Name.Set.to_list_map alias ~f:(Alias.make ~dir) in
  match test with
  | Error (Missing_run_t test) ->
    (* We error out on invalid tests even if they are disabled. *)
    Memo.parallel_iter aliases ~f:(fun alias ->
      Alias_rules.add sctx ~alias ~loc (missing_run_t test))
  | Ok test ->
    (* Morally, this is equivalent to evaluating them all concurrently and
       taking the conjunction, but we do it this way to avoid evaluating things
       unnecessarily *)
    Memo.List.for_all enabled_if ~f:(fun (expander, blang) ->
      Expander.eval_blang expander blang)
    >>= (function
     | false ->
       Memo.parallel_iter aliases ~f:(fun alias -> Alias_rules.add_empty sctx ~alias ~loc)
     | true ->
       let cram =
         let open Action_builder.O in
         let prefix_with, _ = Path.Build.extract_build_context_dir_exn dir in
         let script = Path.Build.append_source prefix_with (Cram_test.script test) in
         let+ () = Action_builder.path (Path.build script)
         and+ () = Action_builder.all_unit deps
         and+ () =
           match test with
           | File _ -> Action_builder.return ()
           | Dir { dir; file = _ } ->
             let deps =
               Path.Build.append_source prefix_with dir |> Path.build |> Source_deps.files
             in
             let+ (_ : Path.Set.t) = Action_builder.dyn_memo_deps deps in
             ()
         and+ locks = locks >>| Path.Set.to_list in
         Action.progn
           [ Cram_exec.action (Path.build script)
           ; Diff
               { Diff.optional = true
               ; mode = Text
               ; file1 = Path.build script
               ; file2 = Path.Build.extend_basename script ~suffix:".corrected"
               }
           ]
         |> Action.Full.make ~locks ~sandbox
       in
       Memo.parallel_iter aliases ~f:(fun alias -> Alias_rules.add sctx ~alias ~loc cram))
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
  let open Memo.O in
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
      let init =
        ( None
        , let alias = Alias.Name.of_string name |> Alias.Name.Set.add Spec.empty.alias in
          { Spec.empty with alias } )
      in
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
              let alias =
                match stanza.alias with
                | None -> acc.alias
                | Some a -> Alias.Name.Set.add acc.alias a
              in
              let packages =
                match stanza.package with
                | None -> acc.packages
                | Some (p : Package.t) ->
                  Package.Name.Set.add acc.packages (Package.name p)
              in
              ( runtest_alias
              , { acc with enabled_if; locks; deps; alias; packages; sandbox } ))
      in
      let alias =
        let to_add =
          match runtest_alias with
          | None | Some (_, true) -> Alias.Name.Set.singleton Alias0.runtest
          | Some (_, false) -> Alias.Name.Set.empty
        in
        Alias.Name.Set.union to_add acc.alias
      in
      { acc with alias }
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
