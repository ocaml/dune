Executable that tries to use two implementations for the same virtual lib
  $ dune build
  Error: Conflicting implementations for virtual library "vlib" in
  _build/default/vlib:
  - "impl1" in _build/default/impl1
    -> required by library "bar" in _build/default
  - "impl2" in _build/default/impl2
  This cannot work.
  -> required by executable foo in dune:2
  -> required by _build/default/foo.exe
  -> required by alias default in dune:11
  [1]
