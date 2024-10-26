open Import

let runtest_info =
  let doc = "Run tests." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is a short-hand for calling:|}
    ; `Pre {|  dune build @runtest|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ ( "Run all tests in the current source tree (including those that passed on \
             the last run)"
          , "dune runtest --force" )
        ; ( "Run tests sequentially without output buffering"
          , "dune runtest --no-buffer -j 1" )
        ]
    ]
  in
  Cmd.info "runtest" ~doc ~man ~envs:Common.envs
;;

let runtest_term =
  let name_ = Arg.info [] ~docv:"DIR" in
  let+ builder = Common.Builder.term
  and+ dirs = Arg.(value & pos_all string [ "." ] name_) in
  let common, config = Common.init builder in
  let request (setup : Import.Main.build_system) =
    Action_builder.all_unit
      (List.map dirs ~f:(fun dir ->
         let dir = Path.(relative root) (Common.prefix_target common dir) in
         Alias.in_dir
           ~name:Dune_rules.Alias.runtest
           ~recursive:true
           ~contexts:setup.contexts
           dir
         |> Alias.request))
  in
  Build_cmd.run_build_command ~common ~config ~request
;;

let commands =
  let command = Cmd.v runtest_info runtest_term in
  [ command; command_alias command runtest_term "test" ]
;;
