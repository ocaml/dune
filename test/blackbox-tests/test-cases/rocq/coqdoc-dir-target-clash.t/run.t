We try to build the documentation but there will be a clash between the
directory targets.
  $ dune build @check
  File "dune", lines 1-7, characters 0-120:
  1 | (rule
  2 |  (targets
  3 |   (dir base.html))
  4 |  (action
  5 |   (progn
  6 |    (run mkdir base.html)
  7 |    (run touch base.html/base.base.html))))
  Error: The following both define the same directory target:
  _build/default/base.html
  - dune:1
  - dune:17
  [1]
