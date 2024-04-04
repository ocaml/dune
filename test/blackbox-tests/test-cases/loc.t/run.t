In this test we want to check the output of the rule reporting.

  $ dune build
  File "dune", lines 1-16, characters 0-154:
   1 | (rule
   2 |  (deps idontexist)
   3 |  ;
  ....
  14 |  (targets bar)
  15 |  (action
  16 |   (run idontexist)))
  Error: No rule found for idontexist
  [1]
