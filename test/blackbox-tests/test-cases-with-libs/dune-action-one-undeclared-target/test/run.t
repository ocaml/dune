  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest (exit 2)
  (cd _build/default && ./foo.exe)
  Fatal error: exception Failure("bar is written despite not being declared as a target in dune file. To fix, add it to target list in dune file.")
  Raised at file "stdlib.ml", line 29, characters 17-33
  Called from file "src/dune_action/dune_action.ml", line 124, characters 6-187
  Called from file "test/blackbox-tests/test-cases-with-libs/dune-action-one-undeclared-target/bin/foo.ml", line 8, characters 9-19
  [1]
