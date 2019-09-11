This test checks that 'dynamic-run' will not be reexecuted if
dependencies do not change (have the same digest) even if
they were forced to rebuild.

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target bar)
  >  (deps (universe))
  >  (action
  >   (progn
  >   (echo "Building bar!\n")
  >   (with-stdout-to %{target} (echo "Hello from bar!")))))
  > \
  > (alias
  >  (name runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest --display short
           foo alias runtest
  Building bar!
           foo alias runtest
  Hello from bar!

  $ dune runtest --display short
  Building bar!
