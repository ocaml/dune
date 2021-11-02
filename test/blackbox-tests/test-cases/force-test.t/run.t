Running a test and then forcing a re-run will only re-run the test exe:

  $ dune runtest
  Foo Bar
  $ dune runtest

Note that nothing is rebuilt, only the binary is executed again:
  $ dune runtest --force
  Foo Bar
