  $ cat > dune-project << EOF
  > (lang dune 3.25)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  File "dune", lines 1-3, characters 0-57:
  1 | (rule
  2 |  (alias runtest)
  3 |  (action (dynamic-run ./foo.exe)))
  The directory target "bar" was produced despite not being declared in the dune file. To fix this, declare it as a target.
  [1]
