Builds Menhir parsers with `--cmly` enabled.

  $ cp ../parser-fixtures/lexer1.mll ../parser-fixtures/test_menhir1.mly .
  $ dune build ./test.exe --debug-dependency-path
  $ ls _build/default/test.exe
  _build/default/test.exe
