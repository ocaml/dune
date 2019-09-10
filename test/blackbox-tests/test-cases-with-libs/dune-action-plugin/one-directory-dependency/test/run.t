This test checks that executable that uses 'dynamic-run'
and depends on directory listing forces all targets in that
directory to be build.

  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest
           foo alias runtest
  bar1
  bar2
  bar3
  dune
