Testing a bug with npm creating a directory target with a symlink inside but
Dune not recognizing it

  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias fakenode)
  >  (targets
  >   (dir fakenode_modules))
  >  (action
  >   (run ./fakenpm.exe)))
  > EOF

  $ dune build @fakenode
  File "dune", line 1, characters 0-92:
  1 | (rule
  2 |  (alias fakenode)
  3 |  (targets
  4 |   (dir fakenode_modules))
  5 |  (action
  6 |   (run ./fakenpm.exe)))
  Error: This rule defines a directory target "fakenode_modules" that matches
  the requested path "fakenode_modules" but the rule's action didn't produce it
  -> required by alias fakenode
  [1]
