  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target bar)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > \
  > (alias
  >  (name runtest)
  >  (action (cat bar)))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo bar
  Hello from bar!
