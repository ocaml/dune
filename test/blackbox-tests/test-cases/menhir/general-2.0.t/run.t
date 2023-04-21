Test the menhir extension version 2.0

  $ dune build ./src/test.exe --debug-dependency-path
  $ ls _build/default/src/test.exe
  _build/default/src/test.exe

Check for .conflicts files in stanza with --explain

  $ ls _build/default/src/test_base.conflicts
  _build/default/src/test_base.conflicts
