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
  File "dune", line 35, characters 12-15:
  35 |  (libraries foo))
                   ^^^
  Error: Library "foo" in _build/default is hidden (unsatisfied 'enabled_if').
  -> required by library "bar" in _build/default
  -> required by executable main in dune:44
  -> required by _build/default/main.exe
  [1]
