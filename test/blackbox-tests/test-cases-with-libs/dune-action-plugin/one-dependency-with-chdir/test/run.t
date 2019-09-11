This test checks that 'dynamic-run' can work
when we 'chdir' into different directory.

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (alias
  >  (name runtest)
  >  (action
  >   (chdir foodir
  >   (dynamic-run ./foo.exe))))
  > EOF

  $ mkdir foodir

  $ cat > foodir/dune << EOF
  > (rule
  >  (target bar)
  >  (action
  >   (with-stdout-to %{target} (echo "Hello from bar!"))))
  > EOF

  $ cp ../bin/foo.exe ./foodir/

  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  Hello from bar!
