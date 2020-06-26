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
           foo alias runtest (exit 1)
  (cd _build/default && ./foo.exe)
  bar is written despite not being declared as a target in dune file. To fix, add it to target list in dune file.
  [1]
