@all builds user defined rules

  $ dune build @all

  $ dune trace cat | jq -c '
  > include "dune";
  > progMatching("echo")
  > '
  {"prog":"echo","process_args":["foobar"],"target_files":["_build/default/foo"]}
