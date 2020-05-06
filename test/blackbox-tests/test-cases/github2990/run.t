Test for problem in #2990

  $ dune runtest
  File "dune", line 9, characters 0-191:
   9 | (rule
  10 |  (alias runtest)
  11 |  (action
  12 |   (progn
  13 |    (with-stdout-to output.expected (echo "MESSAGE\n"))
  14 |    (with-stdout-to output.actual (run ./print.exe))
  15 |    (diff? output.expected output.actual))))
  Error: Rule failed to generate the following targets:
  - output.actual
  [1]
