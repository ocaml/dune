  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (data_only_dirs foodir)
  > \
  > (alias
  >  (name runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ mkdir foodir
  $ touch foodir/foo

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  foo
