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
        ]
    ]
  in
  Cmd.info "runtest" ~doc ~man ~envs:Common.envs
;;

let find_cram_test path ~parent_dir =
  let open Memo.O in
  Source_tree.nearest_dir parent_dir
  >>= Dune_rules.Cram_rules.cram_tests
  (* We ignore the errors we get when searching for cram tests as they will
     be reported during building anyway. We are only interested in the
     presence of cram tests. *)
  >>| List.filter_map ~f:Result.to_option
  (* We search our list of known cram tests for the test we are looking
     for. *)
  >>| List.find ~f:(fun (test : Dune_rules.Cram_test.t) ->
    let src =
      match test with
      | File src -> src
      | Dir { dir = src; _ } -> src
    in
    Path.Source.equal path src)
;;

let explain_unsuccessful_search path ~parent_dir =
  let open Memo.O in
  (* If the user misspelled the test name, we give them a hint. *)
  let+ hints =
    (* We search for all files and directories in the parent directory and
       suggest them as possible candidates. *)
    let+ candidates =
      let+ file_candidates =
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
      in
      List.concat [ file_candidates; dir_candidates ]
    in
    User_message.did_you_mean (Path.Source.to_string path) ~candidates
  in
  User_error.raise
    ~hints
    [ Pp.textf "%S does not match any known test." (Path.Source.to_string path) ]
;;

(* [disambiguate_test_name path] is a function that takes in a
   directory [path] and classifies it as either a cram test or a directory to
   run tests in. *)
let disambiguate_test_name path =
  match Path.Source.parent path with
  | None -> Memo.return @@ `Runtest (Path.source Path.Source.root)
  | Some parent_dir ->
    let open Memo.O in
    find_cram_test path ~parent_dir
    >>= (function
     | Some test ->
       (* If we find the cram test, then we request that is run. *)
       Memo.return (`Test (parent_dir, Dune_rules.Cram_test.name test))
     | None ->
       (* If we don't find it, then we assume the user intended a directory for
          @runtest to be used. *)
       Source_tree.find_dir path
       >>= (function
        (* We need to make sure that this directory or file exists. *)
        | Some _ -> Memo.return (`Runtest (Path.source path))
        | None -> explain_unsuccessful_search path ~parent_dir))
;;

let runtest_term =
  let name = Arg.info [] ~docv:"TEST" in
  let+ builder = Common.Builder.term
  and+ dirs = Arg.(value & pos_all string [ "." ] name) in
  let common, config = Common.init builder in
  let request (setup : Import.Main.build_system) =
    List.map dirs ~f:(fun dir ->
      let dir = Path.(relative root) (Common.prefix_target common dir) in
      let open Action_builder.O in
      let* alias_kind =
        match Path.as_in_source_tree dir with
        (* If the path is in the source tree, we disambiguate it. *)
        | Some path -> Action_builder.of_memo (disambiguate_test_name path)
        | None -> Action_builder.return (`Runtest dir)
      in
      Alias.request
      @@
      match alias_kind with
      | `Test (dir, alias_name) ->
        Alias.in_dir
          ~name:(Dune_engine.Alias.Name.of_string alias_name)
          ~recursive:false
          ~contexts:setup.contexts
          (Path.source dir)
      | `Runtest dir ->
        Alias.in_dir
          ~name:Dune_rules.Alias.runtest
          ~recursive:true
          ~contexts:setup.contexts
          dir)
    |> Action_builder.all_unit
  in
  Build_cmd.run_build_command ~common ~config ~request
;;

let commands =
  let command = Cmd.v runtest_info runtest_term in
  [ command; command_alias command runtest_term "test" ]
;;
