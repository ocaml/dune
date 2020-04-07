Test that `enabled_if` fields work as expected.

This alias is disabled, building it should do nothing:
  $ dune build @x

This one is enabled:
  $ dune build @y
  Building alias y

This rule is disabled, trying to build a should fail:
  $ dune build a
  Error: Don't know how to build a
  Hint: did you mean b?
  [1]

This one is enabled:
  $ dune build b
  Building file b

Test the enabled_if field for libraries:

  $ dune build foo
  Error: Don't know how to build foo
  [1]

  $ dune build main.exe
  File "dune", line 33, characters 12-15:
  33 |  (libraries foo))
                   ^^^
  Error: Library "foo" in _build/default is hidden (unsatisfied 'enabled_if').
  Hint: try: dune external-lib-deps --missing main.exe
  [1]

Ideally, the above message should mention the dependency path between
the requested target and the unsatisfied `enabled_if`.

Since dune 2.5 libraries `enabled_if` can use the `%{ocaml_version}` variable.

This library is enabled (all versions)
  $ dune build main.exe --root ocaml_version
  Entering directory 'ocaml_version'

This one is disabled (version too low)
  $ dune build main2.exe --root ocaml_version
  Entering directory 'ocaml_version'
  File "dune", line 27, characters 13-23:
  27 |   (libraries futurecaml))
                    ^^^^^^^^^^
  Error: Library "futurecaml" in _build/default is hidden (unsatisfied
  'enabled_if').
  Hint: try: dune external-lib-deps --missing --root ocaml_version main2.exe
  [1]
