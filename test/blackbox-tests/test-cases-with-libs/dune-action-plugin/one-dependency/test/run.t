This test checks that executable that uses 'dynamic-run'
and requires one dependency can be successfully run.

  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  Hello from bar!
