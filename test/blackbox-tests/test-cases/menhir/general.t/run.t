Build and run a source file that requires a menhir parser.

  $ cat >dune-project <<EOF
  > (lang dune 1.0)
  > (using menhir 1.0)
  > EOF

  $ dune build ./src/test.exe --debug-dependency-path
  $ ls _build/default/src/test.exe
  _build/default/src/test.exe

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (using menhir 2.0)
  > EOF

  $ dune build ./src/test.exe --debug-dependency-path
  File "command line", line 1:
  Error: Unbound module Dune__exe
  -> required by src/test_menhir1__mock.mli.inferred
  -> required by src/test_menhir1.mli
  -> required by src/.test.eobjs/test_menhir1.mli.d
  -> required by src/.test.eobjs/dune__exe__Test_menhir1.intf.all-deps
  -> required by src/.test.eobjs/dune__exe__Lexer1.impl.all-deps
  -> required by src/test.exe
  [1]
  $ ls _build/default/src/test.exe
  _build/default/src/test.exe
