  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

Reading a directory containing the action's target is rejected.

  $ timeout 3 dune build some_file
  File "dune", lines 1-4, characters 0-62:
  1 | (rule
  2 |  (target some_file)
  3 |  (action
  4 |   (dynamic-run ./foo.exe)))
  Dependency cycle between:
     _build/default/some_file
  [1]
