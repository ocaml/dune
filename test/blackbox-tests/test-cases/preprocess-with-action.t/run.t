This test check that we can preprocess source code with actions

  $ dune runtest

  $ dune trace cat | jq '
  > include "dune";
  >   processes
  > | .args
  > | select(.prog | contains("pp.exe"))
  > | {prog, target_files, dir, exit}
  > '
  {
    "prog": "pp/pp.exe",
    "target_files": [
      "_build/default/dune/test.pp.ml"
    ],
    "dir": "_build/default",
    "exit": 0
  }
