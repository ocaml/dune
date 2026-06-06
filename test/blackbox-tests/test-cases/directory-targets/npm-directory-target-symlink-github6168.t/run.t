Testing a bug with npm creating a directory target with a symlink inside but
Dune not recognizing it

  $ make_directory_targets_project 3.5

  $ cat > dune << EOF
  > (rule
  >  (alias fakenode)
  >  (targets
  >   (dir fakenode_modules))
  >  (action
  >   (run ./fakenpm.exe)))
  > EOF
  dune: Permission denied
  [1]

  $ dune build @fakenode
  Error: Alias "fakenode" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]
