  $ dune build
  File "dune", line 20, characters 12-22:
  20 |  (libraries lte414caml))
                   ^^^^^^^^^^
  Error: Library "lte414caml" in _build/default is hidden (unsatisfied
  'enabled_if').
  -> required by _build/default/.main.eobjs/byte/dune__exe__Main.cmi
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  -> required by alias all
  -> required by alias default
  [1]
