  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest
  File "dune", line 1, characters 0-57:
  1 | (alias
  2 |  (name runtest)
  3 |  (action (run-dynamic ./foo.exe)))
  Error: No rule found for bar
  [1]
