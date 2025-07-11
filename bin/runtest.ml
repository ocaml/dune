open Import

let runtest_info =
  let doc = "Run tests." in
  let man =
    [ `S "DESCRIPTION"
    ; `P "Run the given tests. The [TEST] argument can be either:"
    ; `I
        ( "-"
        , "A directory: If a directory is provided, dune will recursively run all tests \
           within that directory." )
    ; `I
        ( "-"
        , "A file name: If a specific file name is provided, dune will run the tests \
           with that name." )
    ; `I ("-", "A library name with (inline_tests)")
    ; `P
        "If no [TEST] is provided, dune will run all tests in the current directory and \
         its subdirectories."
    ; `P "See EXAMPLES below for additional information on use cases."
    ; `Blocks Common.help_secs
    ; Common.examples
        [ "Run all tests in a given directory", "dune runtest path/to/dir/"
        ; "Run a specific cram test", "dune runtest path/to/mytest.t"
        ; ( "Run all tests in the current source tree (including those that passed on \
             the last run)"
          , "dune runtest --force" )
        ; ( "Run tests sequentially without output buffering"
          , "dune runtest --no-buffer -j 1" )
        ; "Run tests in a specific build context", "dune runtest _build/my_context/"
        ; "Run inline tests of a library", "dune runtest src/my_lib_with_tests"
        ]
    ]
  in
  Cmd.info "runtest" ~doc ~man ~envs:Common.envs
;;

let find_cram_tests_in_dir (dir : Path.Source.t) =
  let open Memo.O in
  Source_tree.nearest_dir dir
  >>= Dune_rules.Cram_rules.cram_tests
  (* We ignore the errors we get when searching for cram tests as they will
     be reported during building anyway. We are only interested in the presence
     of cram tests. *)
  >>| List.filter_map ~f:Result.to_option
;;

let find_cram_test (path : Path.Build.t) =
  let open Memo.O in
  let path = Path.Build.drop_build_context_exn path in
  let parent_dir = Path.Source.parent_exn path in
  find_cram_tests_in_dir parent_dir
  >>| List.find_map ~f:(fun (test : Source.Cram_test.t) ->
    let src =
      match test with
      | File src -> src
      | Dir { dir = src; _ } -> src
    in
    Option.some_if
      (Path.Source.equal path src)
      (`Test (Path.source parent_dir, Source.Cram_test.name test)))
;;

let find_inline_tests_in_dir dir =
  let open Memo.O in
  Dune_rules.Dune_load.stanzas_in_dir dir
  >>= Memo.Option.map ~f:(fun dune_file ->
    Dune_file.find_stanzas dune_file Dune_rules.Library.key
    >>| List.filter_map ~f:(fun { Library.sub_systems; name = _loc, name; _ } ->
      Option.some_if
        (Dune_rules.Sub_system_name.Map.mem
           sub_systems
           (Dune_rules.Sub_system_name.of_string "inline_tests"))
        name))
  >>| Option.to_list
  >>| List.concat
;;

let find_inline_test (path : Path.Build.t) =
  let name = Path.Build.basename path in
  let parent_dir = Path.Build.parent_exn path in
  let open Memo.O in
  find_inline_tests_in_dir parent_dir
  >>| List.find_map ~f:(fun lib_name ->
    Option.some_if
      (String.equal name (Lib_name.Local.to_string lib_name))
      (`Test (Path.build parent_dir, "runtest-" ^ name)))
;;

let find_stanza_tests_in_dir dir =
  let open Memo.O in
  Dune_rules.Dune_load.stanzas_in_dir dir
  >>= Memo.Option.map ~f:(fun dune_file ->
    Dune_file.find_stanzas dune_file Dune_rules.Tests.key
    >>| List.concat_map ~f:(fun { Dune_rules.Tests.exes; _ } ->
      exes.Executables.names |> Nonempty_list.to_list |> List.map ~f:snd))
  >>| Option.to_list
  >>| List.concat
;;

let find_stanza_test (path : Path.Build.t) =
  let name = Path.Build.basename path in
  let parent_dir = Path.Build.parent_exn path in
  let open Memo.O in
  find_stanza_tests_in_dir parent_dir
  >>| List.find_map ~f:(fun test_stanza_name ->
    Option.some_if
      (String.equal name test_stanza_name)
      (`Test (Path.build parent_dir, "runtest-" ^ name)))
;;

let search_for_test (path : Path.Build.t) =
  [ find_cram_test; find_inline_test; find_stanza_test ]
  |> Memo.List.find_map ~f:(fun f -> f path)
;;

let disambiguate_test_name (path : Path.Build.t) =
  Action_builder.of_memo
  @@
  (* It's important we check the parent of the path with the dropped prefix
     otherwise we would count _build as a valid parent. *)
  match Path.Build.drop_build_context_exn path |> Path.Source.parent with
  | None -> Memo.return @@ `Runtest (Path.source Path.Source.root)
  | Some _parent_dir ->
    let open Memo.O in
    search_for_test path
    >>= (function
     | Some test_requests -> Memo.return test_requests
     | None ->
       (* If we don't find it, then we assume the user intended a directory for
          @runtest to be used. *)
       let path = Path.Build.drop_build_context_exn path in
       Source_tree.find_dir path
       >>| (function
        (* We need to make sure that this directory or file exists. *)
        | Some _ -> `Runtest (Path.source path)
        | None -> `Not_found))
;;

let explain_unsuccessful_search (path : Path.t) contexts =
  Action_builder.of_memo
  @@
  let path = Path.drop_optional_build_context_src_exn path in
  let parent_dir = Path.Source.parent_exn path in
  let open Memo.O in
  (* If the user misspelled the test name, we give them a hint. *)
  let+ hints =
    (* We search for all files and directories in the parent directory and
       suggest them as possible candidates. *)
    let+ candidates =
      let+ file_candidates =
        (* CR alizter: only cram tests should be suggested *)
        let+ files = Source_tree.files_of parent_dir in
        Path.Source.Set.to_list_map files ~f:Path.Source.to_string
      and+ dir_candidates =
        let* parent_source_dir = Source_tree.find_dir parent_dir in
        match parent_source_dir with
        | None -> Memo.return []
        | Some parent_source_dir ->
          let dirs = Source_tree.Dir.sub_dirs parent_source_dir in
          String.Map.to_list dirs
          |> Memo.List.map ~f:(fun (_candidate, candidate_path) ->
            Source_tree.Dir.sub_dir_as_t candidate_path
            >>| Source_tree.Dir.path
            >>| Path.Source.to_string)
      and+ lib_candidates =
        Memo.List.concat_map contexts ~f:(fun context ->
          let dir =
            Path.Build.append_source (Context.build_dir context) path
            |> Path.Build.parent_exn
          in
          find_inline_tests_in_dir dir >>| List.map ~f:Lib_name.Local.to_string)
      and+ test_stanza_candidates =
        Memo.List.concat_map contexts ~f:(fun context ->
          let dir =
            Path.Build.append_source (Context.build_dir context) path
            |> Path.Build.parent_exn
          in
          find_stanza_tests_in_dir dir)
      in
      List.concat
        [ file_candidates; dir_candidates; lib_candidates; test_stanza_candidates ]
    in
    User_message.did_you_mean (Path.Source.to_string path) ~candidates
  in
  User_error.raise
    ~hints
    [ Pp.textf "%S does not match any known test." (Path.Source.to_string path) ]
;;

let runtest_term =
  let name = Arg.info [] ~docv:"TEST" in
  let+ builder = Common.Builder.term
  and+ dirs = Arg.(value & pos_all string [ "." ] name) in
  let common, config = Common.init builder in
  let request (setup : Import.Main.build_system) =
    List.map dirs ~f:(fun dir ->
      let dir = Path.of_string dir |> Path.Expert.try_localize_external in
      let open Action_builder.O in
      let* context_and_alias_kind =
        let contexts = setup.contexts in
        match (Util.check_path contexts dir : Util.checked) with
        | In_build_dir (context, _) ->
          let+ res = disambiguate_test_name (Path.as_in_build_dir_exn dir) in
          [ context, res ]
        | In_source_dir dir ->
          Action_builder.List.map contexts ~f:(fun context ->
            let+ res =
              disambiguate_test_name
                (Path.Build.append_source (Context.build_dir context) dir)
            in
            context, res)
        | In_private_context _ | In_install_dir _ ->
          User_error.raise
            [ Pp.textf
                "This path is internal to dune: %s"
                (Path.to_string_maybe_quoted dir)
            ]
        | External _ ->
          User_error.raise
            [ Pp.textf
                "This path is outside the workspace: %s"
                (Path.to_string_maybe_quoted dir)
            ]
      in
      List.map context_and_alias_kind ~f:(fun (context, alias_kind) ->
        let* alias =
          match alias_kind with
          | `Test (dir, alias_name) ->
            Action_builder.return
            @@ Alias.in_dir
                 ~name:(Dune_engine.Alias.Name.of_string alias_name)
                 ~recursive:false
                 ~contexts:[ context ]
                 dir
          | `Runtest dir ->
            Action_builder.return
            @@ Alias.in_dir
                 ~name:Dune_rules.Alias.runtest
                 ~recursive:true
                 ~contexts:[ context ]
                 dir
          | `Not_found -> explain_unsuccessful_search dir setup.contexts
        in
        Alias.request alias)
      |> Action_builder.all_unit)
    |> Action_builder.all_unit
  in
  Build.run_build_command ~common ~config ~request
;;

let commands =
  let command = Cmd.v runtest_info runtest_term in
  [ command; command_alias command runtest_term "test" ]
;;
