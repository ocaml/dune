This test checks that executable that uses 'dynamic-run'
and requires one dependency can be successfully run.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_dependency)
  >  (action (with-stdout-to %{target} (echo "Hello from some_dependency!"))))
  > \
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  Hello from some_dependency!
