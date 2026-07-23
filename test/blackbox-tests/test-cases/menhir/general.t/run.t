Build and run a source file that requires a menhir parser.

  $ make_menhir_project 1.0 1.0

  $ dune build ./src/test.exe --debug-dependency-path
  $ ls _build/default/src/test.exe
  _build/default/src/test.exe

  $ make_menhir_project 2.0 2.0

  $ dune build ./src/test.exe --debug-dependency-path
  $ ls _build/default/src/test.exe
  _build/default/src/test.exe
