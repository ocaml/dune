This test checks that dune can gracefully handle situation when user provides
ordinary executable instead of one linked against dune-action-plugin.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  Hello from foo!
  File "dune", lines 1-3, characters 0-57:
  1 | (rule
  2 |  (alias runtest)
  3 |  (action (dynamic-run ./foo.exe)))
  Error: Executable 'foo.exe' declared as using dune-action-plugin (declared
  with 'dynamic-run' tag) failed to respond to dune.
  
  If you don't use dynamic dependency discovery in your executable you may
  consider changing 'dynamic-run' to 'run' in your rule definition.
  [1]

