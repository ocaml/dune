This test checks that 'dynamic-run' will not be reexecuted if
dependencies do not change (have the same digest) even if
they were forced to rebuild.

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (deps (universe))
  >  (action
  >   (progn
  >   (echo "Building some_file!\n")
  >   (with-stdout-to %{target} (echo "Hello from some_file!")))))
  > \
  > (alias
  >  (name runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo alias runtest
  Building some_file!
           foo alias runtest
  Hello from some_file!

  $ dune runtest --display short
  Building some_file!
