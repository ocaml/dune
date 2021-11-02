This test checks that 'dynamic-run' can work
when we 'chdir' into different directory.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (chdir some_dir
  >    (dynamic-run ./foo.exe))))
  > EOF

  $ mkdir some_dir

  $ cat > some_dir/dune << EOF
  > (rule
  >  (target some_dependency)
  >  (action
  >   (with-stdout-to %{target} (echo "Hello from some_dependency!"))))
  > EOF

  $ cp ./bin/foo.exe ./some_dir
  $ dune runtest
  Hello from some_dependency!
