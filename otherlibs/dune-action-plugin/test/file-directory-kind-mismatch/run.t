  $ cat > dune-project << EOF
  > (lang dune 3.25)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (targets output)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > \
  > (rule
  >  (alias runtest)
  >  (action (cat output)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  File "dune", lines 1-4, characters 0-60:
  1 | (rule
  2 |  (targets output)
  3 |  (action
  4 |   (dynamic-run ./foo.exe)))
  The directory target "output" was produced, but "output" is declared as a file target in the dune file.
  [1]
