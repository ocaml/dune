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
  and+ tests_to_run = Arg.(value & pos_all string [] name)
  and+ aliases = Common.alias_flags_term in
  let common, config = Common.init builder in
  let tests_to_run =
    match tests_to_run, aliases with
    | [], [] -> [ "." ]
    | _ -> tests_to_run
  in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Ok () ->
    let request setup =
      Action_builder.all_unit
        [ Runtest_common.make_request
            ~dir_or_cram_test_paths:tests_to_run
            ~to_cwd:(Common.root common).to_cwd
            setup
        ; Target.interpret_targets (Common.root common) config setup aliases
        ]
    in
    Build.run_build_command ~common ~config ~request
  | Error lock_held_by ->
    let combined_request () =
      (* CR-someday Alizter: We should be able to fire multiple requests at the
         same time. *)
      let open Fiber.O in
      (* CR-someday Alizter: Find out the best value for this or fix the buggy
         behaviouir. Polling forever causes issues that become apparent here
         due to our double request. We therefore diable it here. *)
      let wait = false in
      let runtest_request =
        if List.is_empty tests_to_run
        then Fiber.return (Ok Dune_rpc.Build_outcome_with_diagnostics.Success)
        else
          Rpc.Rpc_common.fire_request
            ~name:"runtest"
            ~wait
            ~lock_held_by
            builder
            Dune_rpc.Procedures.Public.runtest
            tests_to_run
      in
      let build_request =
        if List.is_empty aliases
        then Fiber.return (Ok Dune_rpc.Build_outcome_with_diagnostics.Success)
        else
          Rpc.Rpc_common.fire_request
            ~name:"build"
            ~wait
            ~lock_held_by
            builder
            Dune_rpc_impl.Decl.build
            (Rpc.Rpc_common.prepare_targets aliases)
      in
      let+ runtest_result = runtest_request
      and+ build_result = build_request in
      match runtest_result, build_result with
      | Ok Success, Ok Success -> Ok Dune_rpc.Build_outcome_with_diagnostics.Success
      | Ok (Failure errors1), Ok (Failure errors2) -> Ok (Failure (errors1 @ errors2))
      | Ok (Failure errors), Ok Success | Ok Success, Ok (Failure errors) ->
        Ok (Failure errors)
      (* CR-someday Alizter: This is wrong and we drop errors here. Find a way
         to combine them. *)
      | Error e, _ | _, Error e -> Error e
    in
    Rpc.Rpc_common.wrap_build_outcome_exn ~print_on_success:true combined_request ()
    |> Scheduler.go_without_rpc_server ~common ~config
;;

let commands =
  let command = Cmd.v runtest_info runtest_term in
  [ command; command_alias command runtest_term "test" ]
;;
