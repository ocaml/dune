When a Coq .v file has a `Load "file.v"` statement, we check that dune correctly
finds the file and declares it as a dep.

  $ dune build
