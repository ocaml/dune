@all builds user defined rules

  $ dune build @all

  $ dune trace cat | jq_dune -c '
  > progMatching("echo")
  > '
  {"prog":"echo","process_args":["foobar"],"target_files":["_build/default/foo"]}
