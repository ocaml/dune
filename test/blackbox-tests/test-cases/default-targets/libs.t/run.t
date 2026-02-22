Generate targets when modes are set for libraries

  $ dune build @all

  $ dune trace cat | jq -c '
  > include "dune"; targetsMatchingFilter(test("\\.cm(a|xa|xs)$"))
  > '
  {"target_files":["_build/default/byteandnative.cma"]}
  {"target_files":["_build/default/byteonly.cma"]}
  {"target_files":["_build/default/byteandnative.a","_build/default/byteandnative.cmxa"]}
  {"target_files":["_build/default/nativeonly.a","_build/default/nativeonly.cmxa"]}
  {"target_files":["_build/default/byteandnative.cmxs"]}
  {"target_files":["_build/default/nativeonly.cmxs"]}
