An old-style directory target is rejected before promotion.

  $ dune build @all
  File "dune", lines 1-4, characters 0-73:
  1 | (rule
  2 |  (targets dir)
  3 |  (mode promote)
  4 |  (action (bash "mkdir %{targets}")))
  Error: Error trying to read targets after a rule was run:
  - dir: Directory produced for a file target. Use (dir dir).
  [1]
