This test checks that executable that uses 'dynamic-run'
and depends on directory listing forces all targets in that
directory to be build.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
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

  $ cp ./bin/foo.exe ./

  $ dune runtest
  dune
  some_file1
  some_file2
  some_file3
