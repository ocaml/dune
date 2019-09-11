This test checks that executable that uses 'dynamic-run'
and depends on directory listing forces all targets in that
directory to be build.

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (alias
  >  (name runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ mkdir foodir

  $ cat > foodir/dune << EOF
  > (rule
  >  (target bar1)
  >  (action (with-stdout-to %{target} (echo ""))))
  > \
  > (rule
  >  (target bar2)
  >  (action (with-stdout-to %{target} (echo ""))))
  > \
  > (rule
  >  (target bar3)
  >  (action (with-stdout-to %{target} (echo ""))))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  bar1
  bar2
  bar3
  dune
