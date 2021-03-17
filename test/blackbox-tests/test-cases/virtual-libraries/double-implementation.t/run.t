Executable that tries to use two implementations for the same virtual lib
  $ dune build
  File "dune", line 11, characters 0-49:
  11 | (alias
  12 |  (name default)
  13 |  (action (run ./foo.exe)))
  Error: Conflicting implementations for virtual library "vlib" in
  _build/default/vlib:
  - "impl1" in _build/default/impl1
    -> required by library "bar" in _build/default
  - "impl2" in _build/default/impl2
  This cannot work.
  [1]
