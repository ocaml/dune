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
  $ ls _build/default/src/test.exe
  _build/default/src/test.exe
