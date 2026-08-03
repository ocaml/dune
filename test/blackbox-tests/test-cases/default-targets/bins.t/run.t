Generates targets when modes is set for binaries:
  $ dune build @all

  $ dune trace cat | jq_dune -c '
  > targetsMatchingFilter(test("\\.(bc|exe)$"))
  > ' | sort
  {"target_files":["_build/default/byteandnative.bc"]}
  {"target_files":["_build/default/byteandnative.exe"]}
  {"target_files":["_build/default/bytecodeonly.bc"]}
  {"target_files":["_build/default/bytecodeonly.exe"]}
  {"target_files":["_build/default/nativeonly.exe"]}
