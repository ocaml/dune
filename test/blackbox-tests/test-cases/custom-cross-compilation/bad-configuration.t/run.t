  $ dune build file @install
  File "dune-workspace", line 5, characters 9-50:
  5 | (context (default
  6 |  (name cross-1)
  7 |  (host default)))
  Error: Context 'cross-1' is both a host (for 'cross-2') and a target (for
  'default').
  [1]
