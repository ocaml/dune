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
  Hint: try:
    dune external-lib-deps --missing main2.exe
  [1]
