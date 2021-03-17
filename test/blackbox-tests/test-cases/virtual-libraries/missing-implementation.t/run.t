Executable that tries to build against a virtual library without an implementation
  $ dune build
  File "dune", line 5, characters 0-49:
  5 | (alias
  6 |  (name default)
  7 |  (action (run ./foo.exe)))
  Error: No implementation found for virtual library "vlib" in
  _build/default/vlib.
  [1]
