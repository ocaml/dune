This test checks that 'dynamic-run' will not be reexecuted if
dependencies do not change (have the same digest) even if
they were forced to rebuild.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (deps (universe))
  >  (action
  >   (progn
  >   (echo "Building some_file!\n")
  >   (with-stdout-to %{target} (echo "Hello from some_file!")))))
  > \
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  Building some_file!
  Hello from some_file!

  $ dune runtest
  Building some_file!
