Duplicate directory targets

  $ dune build
  File "dune", lines 1-3, characters 0-53:
  1 | (rule
  2 |  (targets (dir foo))
  3 |  (action (run mkdir foo)))
  Error: The following both define the same directory target:
  _build/default/foo
  - dune:1
  - dune:5
  [1]
