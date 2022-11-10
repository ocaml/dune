The targets should only be interpreted as a single path when quoted

  $ dune build s
  File "dune", line 1, characters 0-72:
  1 | (rule
  2 |  (targets s t)
  3 |  (action (with-stdout-to "%{targets}" (echo foo))))
  Error: Rule failed to generate the following targets:
  - s
  - t
  [1]
