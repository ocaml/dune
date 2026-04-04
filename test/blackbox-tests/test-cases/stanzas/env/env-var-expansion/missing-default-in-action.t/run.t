Make sure that we require a default value regardless of the context

  $ dune build @echo
  File "dune", line 3, characters 15-34:
  3 |  (action (echo %{env:DUNE_ENV_VAR})))
                     ^^^^^^^^^^^^^^^^^^^
  Error: %{env:..} must always come with a default value.
  Hint: the syntax is %{env:VAR=DEFAULT-VALUE}
  [1]
  $ dune build @echo
  File "dune", line 3, characters 15-34:
  3 |  (action (echo %{env:DUNE_ENV_VAR})))
                     ^^^^^^^^^^^^^^^^^^^
  Error: %{env:..} must always come with a default value.
  Hint: the syntax is %{env:VAR=DEFAULT-VALUE}
  [1]
