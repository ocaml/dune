Build and run a source file that requires a menhir parser.

(Running the executable fails, but it doesn't matter since we only care about compilation)

  $ dune exec ./src/test.exe --debug-dependency-path
  Fatal error: exception Failure("lexing: empty token")
  [2]
