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

let runtest_via_rpc_server ~dir_or_cram_test_paths =
  let open Fiber.O in
  let+ response = Rpc.Runtest_rpc.runtest ~wait:true ~dir_or_cram_test_paths in
  match response with
  | Error (error : Dune_rpc_private.Response.Error.t) ->
    Printf.eprintf
      "Error: %s\n%!"
      (Dyn.to_string (Dune_rpc_private.Response.Error.to_dyn error))
  | Ok Success ->
    Console.print_user_message
      (User_message.make [ Pp.text "Success" |> Pp.tag User_message.Style.Success ])
  | Ok (Failure errors) ->
    List.iter errors ~f:(fun { Dune_engine.Compound_user_error.main; _ } ->
      Console.print_user_message main);
    User_error.raise
      [ (match List.length errors with
         | 0 ->
           Code_error.raise
             "Runtest via RPC failed, but the RPC server did not send an error message."
             []
         | 1 -> Pp.textf "Build failed with 1 error."
         | n -> Pp.textf "Build failed with %d errors." n)
      ]
;;

let runtest_term =
  let name = Arg.info [] ~docv:"TEST" in
  let+ builder = Common.Builder.term
  and+ dir_or_cram_test_paths = Arg.(value & pos_all string [ "." ] name) in
  let common, config = Common.init builder in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Error lock_held_by ->
    Scheduler.go_without_rpc_server ~common ~config (fun () ->
      if not (Common.Builder.equal builder Common.Builder.default)
      then
        User_warning.emit
          [ Pp.textf
              "Your build request is being forwarded to a running Dune instance%s so \
               most command-line arguments will be ignored."
              (match (lock_held_by : Dune_util.Global_lock.Lock_held_by.t) with
               | Unknown -> ""
               | Pid_from_lockfile pid -> sprintf " (pid: %d)" pid)
          ];
      runtest_via_rpc_server ~dir_or_cram_test_paths)
  | Ok () ->
    Build.run_build_command
      ~common
      ~config
      ~request:(Runtest_common.make_request ~dir_or_cram_test_paths)
;;

let commands =
  let command = Cmd.v runtest_info runtest_term in
  [ command; command_alias command runtest_term "test" ]
;;
