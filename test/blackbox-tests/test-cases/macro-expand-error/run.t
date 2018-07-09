we're getting an acceptable error message when adding a macro form in an
inappropariate place:

  $ dune build
  Info: creating file dune-project with this contents: (lang dune 1.0)
  File "dune", line 1, characters 14-21:
  Error: %{read:..} isn't allowed in this position
  [1]
