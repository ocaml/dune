This test checks that executable that uses 'dynamic-run'
and requires one dependency can be successfully run.

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target bar)
  >  (action (with-stdout-to %{target} (echo "Hello from bar!"))))
  > \
  > (alias
  >  (name runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  Hello from bar!
