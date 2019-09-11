  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target bar)
  >  (action
  >   (chdir foodir
  >    (dynamic-run ./foo.exe))))
  > \
  > (alias
  >  (name runtest)
  >  (action (cat bar)))
  > EOF

  $ mkdir foodir

  $ cp ../bin/foo.exe ./foodir/

  $ dune runtest --display short
           foo bar
  Hello from bar!
