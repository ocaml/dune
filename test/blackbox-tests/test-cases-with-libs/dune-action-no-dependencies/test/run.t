This test checks that executable that uses  "dune action"
but requires no dependencies can be successfully run.

  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest
  Hello from foo!
