This test checks that 'dynamic-run' will not be reexecuted if
dependencies do not change (have the same digest) even if
they were forced to rebuild.

  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest
  Building bar!
           foo alias runtest
  Hello from bar!
  $ dune runtest --display short
  Building bar!
