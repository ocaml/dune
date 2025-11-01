open Import

(** [make_request ~dir_or_cram_test_paths ~to_cwd] returns a function suitable
    for passing to [Build_cmd.run_build_system] which runs the tests referred
    to by the elements of [dir_or_cram_test_paths]. *)
val make_request
  :  dir_or_cram_test_paths:string list
  -> to_cwd:string list
  -> Dune_rules.Main.build_system
  -> unit Action_builder.t
