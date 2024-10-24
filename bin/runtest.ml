open Import

(** TODO-Ali: update this information for new features of test *)
let info =
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

let term =
  let name_ = Arg.info [] ~docv:"DIR" in
  let+ builder = Common.Builder.term
  and+ dirs = Arg.(value & pos_all string [ "." ] name_) in
  let common, config = Common.init builder in
  let request (setup : Import.Main.build_system) =
    Action_builder.all_unit
      (List.map dirs ~f:(fun dir ->
         let dir = Path.(relative root) (Common.prefix_target common dir) in
         Alias.request
         @@
         let base_dir, ext = Path.split_extension dir in
         let alias_kind =
           match ext, Path.is_directory dir with
           | ".t", true -> `Test (Path.to_string base_dir)
           | ".t", false -> `Test (Path.basename base_dir)
           | _, false -> `Test (Path.to_string dir)
           | _, true -> `Runtest
         in
         match alias_kind with
         | `Test alias_name ->
           Alias.of_string
             (Common.root common)
             ~recursive:true
             ~contexts:setup.contexts
             alias_name
         | `Runtest ->
           Alias.in_dir
             ~name:Dune_rules.Alias.runtest
             ~recursive:true
             ~contexts:setup.contexts
             dir))
  in
  Build_cmd.run_build_command ~common ~config ~request
;;

let command = Cmd.v info term
