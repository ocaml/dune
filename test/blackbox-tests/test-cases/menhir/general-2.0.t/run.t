Test the menhir extension version 2.0

  $ mkdir src
  $ cp ../parser-fixtures/* src/
  $ dune build ./src/test.exe --debug-dependency-path
  $ ls _build/default/src/test.exe
  _build/default/src/test.exe
