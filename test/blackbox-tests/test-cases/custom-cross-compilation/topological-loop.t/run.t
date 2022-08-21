  $ dune build file @install
  File "dune-workspace", line 13, characters 9-50:
  13 | (context (default
  14 |  (name cross-3)
  15 |  (host cross-2)))
  Error: Context 'cross-3' is both a host (for 'cross-1') and a target (for
  'cross-2').
  [1]
