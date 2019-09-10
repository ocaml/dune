Check that multiple 'dynamic-run' commands within single action are
detected and error is printed even if the rule is not executed.

  $ cp ../bin/foo.exe ./
  $ dune runtest
  File "dune", line 4, characters 2-73:
  4 |   (progn
  5 |    (dynamic-run ./foo.exe bar1)
  6 |    (dynamic-run ./foo.exe bar2))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
