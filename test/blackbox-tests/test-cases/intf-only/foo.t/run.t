Successes:

  $ dune build

  $ dune trace cat | jq -c '
  > include "dune";
  > targetsMatchingFilter(test("\\.cma$"))
  > '
  {"target_files":["_build/default/foo.cma"]}
  {"target_files":["_build/default/test/bar.cma"]}
