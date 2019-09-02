  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
  File "dune", line 1, characters 0-107:
  1 | (alias
  2 |  (name runtest)
  3 |  (action
  4 |   (progn
  5 |    (dynamic-run ./foo.exe bar1)
  6 |    (dynamic-run ./foo.exe bar2))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
