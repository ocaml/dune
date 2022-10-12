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
