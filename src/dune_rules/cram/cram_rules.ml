open Import
open Memo.O

module Spec = struct
  type t =
    { loc : Loc.t
    ; alias : Alias.Name.Set.t
    ; deps : unit Action_builder.t list
    ; sandbox : Sandbox_config.t
    ; enabled_if : Blang.t list
    ; locks : Path.Set.t Action_builder.t
    ; packages : Package.Name.Set.t
    }

  let empty =
    { loc = Loc.none
    ; alias = Alias.Name.Set.empty
    ; enabled_if = [ Blang.true_ ]
    ; locks = Action_builder.return Path.Set.empty
    ; deps = []
    ; sandbox = Sandbox_config.needs_sandboxing
    ; packages = Package.Name.Set.empty
    }
  ;;
end

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
  ({ alias; loc; enabled_if; deps; locks; sandbox; packages = _ } : Spec.t)
  (test : (Cram_test.t, Source_tree.Dir.error) result)
  =
  let module Alias_rules = Simple_rules.Alias_rules in
  let aliases = Alias.Name.Set.to_list_map alias ~f:(Alias.make ~dir) in
  match test with
  | Error (Missing_run_t test) ->
    (* We error out on invalid tests even if they are disabled. *)
    Memo.parallel_iter aliases ~f:(fun alias ->
      Alias_rules.add sctx ~alias ~loc (missing_run_t test))
  | Ok test ->
    Expander.eval_blang expander (Blang.And enabled_if)
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
         Action.Full.make action ~locks ~sandbox
       in
       Memo.parallel_iter aliases ~f:(fun alias -> Alias_rules.add sctx ~alias ~loc cram))
;;

let collect_stanzas =
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
  fun ~dir ->
    let* acc = stanzas dir ~f:(fun _ -> true) in
    match Path.Build.parent dir with
    | None -> Memo.return acc
    | Some dir -> collect_whole_subtree [ acc ] dir
;;

let rules ~sctx ~expander ~dir tests =
  let open Memo.O in
  let* stanzas = collect_stanzas ~dir
  and* with_package_mask =
    Only_packages.get_mask ()
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
        | Error (Source_tree.Dir.Missing_run_t test) -> Cram_test.name test
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
              let+ deps, sandbox =
                match stanza.deps with
                | None -> Memo.return (acc.deps, acc.sandbox)
                | Some deps ->
                  let+ (deps : unit Action_builder.t), _, sandbox =
                    let+ expander = Super_context.expander sctx ~dir in
                    Dep_conf_eval.named ~expander deps
                  in
                  deps :: acc.deps, Sandbox_config.inter acc.sandbox sandbox
              in
              let locks =
                (* Locks must be relative to the cram stanza directory and not
                   the individual tests directories *)
                let base = `This (Path.build dir) in
                let open Action_builder.O in
                let+ more_locks =
                  Expander.expand_locks ~base expander stanza.locks
                  |> Action_builder.of_memo
                  >>| Path.Set.of_list
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
              let enabled_if = stanza.enabled_if :: acc.enabled_if in
              let alias =
                match stanza.alias with
                | None -> acc.alias
                | Some a -> Alias.Name.Set.add acc.alias a
              in
              let packages =
                match stanza.package with
                | None -> acc.packages
                | Some (p : Package.t) ->
                  Package.Name.Set.add acc.packages (Package.Id.name p.id)
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
    with_package_mask spec.packages (fun () -> test_rule ~sctx ~expander ~dir spec test))
;;

let rules ~sctx ~expander ~dir tests =
  match tests with
  | [] -> Memo.return ()
  | tests -> rules ~sctx ~expander ~dir tests
;;
