Check that %{targets} is forbidden in preprocessing actions

  $ dune build @all
  File "dune", line 3, characters 37-47:
  3 |  (preprocess (action (with-stdout-to %{targets} (run cat %{input-file})))))
                                           ^^^^^^^^^^
  Warning: Preprocessing actions must not have targets, this target will be
  ignored.
  This will become an error in the future.
  File "dune", line 3, characters 39-47:
  3 |  (preprocess (action (with-stdout-to %{targets} (run cat %{input-file})))))
                                             ^^^^^^^^
  Error: You cannot use %{targets} in preprocessing actions.
  [1]
