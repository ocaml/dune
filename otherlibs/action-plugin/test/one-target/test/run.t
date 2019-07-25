  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target some_target)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > \
  > (alias
  >  (name runtest)
  >  (action (cat some_target)))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo some_target
  Hello from some_target!
