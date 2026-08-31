  $ cat > dune-project << EOF
  > (lang dune 3.25)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (targets (dir output))
  >  (action
  >   (dynamic-run ./foo.exe)))
  > \
  > (rule
  >  (alias runtest)
  >  (action (cat output/some_target)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  Hello from some_target!
