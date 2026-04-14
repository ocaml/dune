Rejects custom cross-compilation contexts with an undefined host.

  $ dune build file @install
  File "dune-workspace", lines 5-7, characters 9-47:
  5 | (context (default
  6 |  (name cross-1)
  7 |  (host oups)))
  Error: Undefined host context 'oups' for 'cross-1'.
  [1]
