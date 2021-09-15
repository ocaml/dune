we're getting an acceptable error message when adding a macro form in an
inappropriate place:

  $ dune build
  File "dune", line 1, characters 12-21:
  1 | (copy_files %{read:x}/*)
                  ^^^^^^^^^
  Error: %{read:..} isn't allowed in this position.
  [1]
