Executable that tries to build against a virtual library without an implementation
  $ dune build
  Error: No implementation found for virtual library "vlib" in
  _build/default/vlib.
  -> required by executable foo in dune:2
  -> required by _build/default/foo.exe
  -> required by alias default in dune:5
  [1]
