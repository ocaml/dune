  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_target)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > \
  > (rule
  >  (alias runtest)
  >  (action (cat some_target)))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo some_target
  Hello from some_target!
