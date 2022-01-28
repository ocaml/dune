Since dune 2.5 libraries `enabled_if` can use the `%{ocaml_version}` variable.

This library is enabled (all versions)
  $ dune build main.exe

This one is disabled (version too low)
  $ dune build main2.exe
  File "dune", line 27, characters 12-22:
  27 |  (libraries futurecaml))
                   ^^^^^^^^^^
  Error: Library "futurecaml" in _build/default is hidden (unsatisfied
  'enabled_if').
  -> required by _build/default/.main2.eobjs/byte/dune__exe__Main2.cmi
  -> required by _build/default/.main2.eobjs/native/dune__exe__Main2.cmx
  -> required by _build/default/main2.exe
  [1]
