This test checks that executable that uses 'dynamic-run'
and depends on directory listing forces all targets in that
directory to be build.

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (alias
  >  (name runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ mkdir some_dir

  $ cat > some_dir/dune << EOF
  > (rule
  >  (target some_file1)
  >  (action (with-stdout-to %{target} (echo ""))))
  > \
  > (rule
  >  (target some_file2)
  >  (action (with-stdout-to %{target} (echo ""))))
  > \
  > (rule
  >  (target some_file3)
  >  (action (with-stdout-to %{target} (echo ""))))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  dune
  some_file1
  some_file2
  some_file3
