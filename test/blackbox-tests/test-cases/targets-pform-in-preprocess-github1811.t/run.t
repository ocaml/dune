Check that %{targets} is forbidden in preprocessing actions

  $ dune build @all
  File "dune", line 3, characters 37-47:
  3 |  (preprocess (action (with-stdout-to %{targets} (run cat %{input-file})))))
                                           ^^^^^^^^^^
  Error: You cannot use %{targets} in preprocessing actions.
  [1]
