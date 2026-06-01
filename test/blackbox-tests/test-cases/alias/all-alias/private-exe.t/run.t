@all builds private exe's

  $ dune build @all

  $ dune trace cat | jq_dune -c '
  >   targetsMatchingFilter(test("\\.exe$"))
  > '
  {"target_files":["_build/default/foo.exe"]}
