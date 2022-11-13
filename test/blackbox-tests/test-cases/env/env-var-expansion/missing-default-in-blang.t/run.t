  $ dune build @echo
  File "dune", line 3, characters 21-40:
  3 |  (enabled_if (= true %{env:DUNE_ENV_VAR}))
                           ^^^^^^^^^^^^^^^^^^^
  Error: %{env:..} must always come with a default value.
  Hint: the syntax is %{env:VAR=DEFAULT-VALUE}
  [1]
