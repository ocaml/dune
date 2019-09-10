This test checks that executable that uses 'dynamic-run'
and requires dependency that can not be build fails.

  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest
  File "dune", line 1, characters 0-57:
  1 | (alias
  2 |  (name runtest)
  3 |  (action (dynamic-run ./foo.exe)))
  Error: No rule found for bar
  [1]
