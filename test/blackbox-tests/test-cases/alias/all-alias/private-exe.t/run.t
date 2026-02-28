@all builds private exe's

  $ dune build @all

  $ dune trace cat | jq -c '
  > include "dune";
  >   targetsMatchingFilter(test("\\.exe$"))
  > '
  {"target_files":["_build/default/foo.exe"]}
