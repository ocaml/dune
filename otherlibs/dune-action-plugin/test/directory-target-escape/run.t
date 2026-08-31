Producing a path that lexically starts with a directory target but escapes it
via [..] is rejected.

  $ cat > dune-project << EOF
  > (lang dune 3.25)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (targets (dir output))
  >  (action
  >   (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune build output
  File "dune", lines 1-4, characters 0-66:
  1 | (rule
  2 |  (targets (dir output))
  3 |  (action
  4 |   (dynamic-run ./foo.exe)))
  The directory target "output/../outside" was produced despite not being declared in the dune file. To fix this, declare it as a target.
  [1]

  $ test ! -e _build/default/outside
