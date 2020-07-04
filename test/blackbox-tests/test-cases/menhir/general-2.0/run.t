Test the menhir extension version 2.0

Note that the binary that we build fails when we run it. This doesn't matter to
the test because we jsut care that we were able to build the binary.

  $ dune exec ./src/test.exe --debug-dependency-path
  Fatal error: exception Failure("lexing: empty token")
  [2]
