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
        ; "Run tests in a specific build context", "dune runtest _build/my_context/"
        ]
    ]
  in
  Cmd.info "runtest" ~doc ~man ~envs:Common.envs
;;

let runtest_term =
  let name = Arg.info [] ~docv:"TEST" in
  let+ builder = Common.Builder.term
  and+ dir_or_cram_test_paths = Arg.(value & pos_all string [ "." ] name) in
  let common, config = Common.init builder in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Ok () ->
    Build.run_build_command
      ~common
      ~config
      ~request:
        (Runtest_common.make_request
           ~dir_or_cram_test_paths
           ~to_cwd:(Common.root common).to_cwd)
  | Error lock_held_by ->
    Scheduler.go_without_rpc_server ~common ~config (fun () ->
      let open Fiber.O in
      let+ build_outcome =
        Rpc.Rpc_common.fire_request
          ~name:"runtest"
          ~wait:false
          ~lock_held_by
          builder
          Dune_rpc.Procedures.Public.runtest
          dir_or_cram_test_paths
      in
      Rpc.Rpc_common.wrap_build_outcome_exn ~print_on_success:true build_outcome)
;;

let commands =
  let command = Cmd.v runtest_info runtest_term in
  [ command; command_alias command runtest_term "test" ]
;;
