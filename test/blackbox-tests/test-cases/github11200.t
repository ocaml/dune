Running `dune subst` should succeed in an empty directory.
Regression test for https://github.com/ocaml/dune/issues/11200

  $ dune subst
  File ".", line 1, characters 0-0:
  Error: There is no dune-project file in the current directory, please add one
  with a (name <name>) field in it.
  Hint: 'dune subst' must be executed from the root of the project.
  [1]
