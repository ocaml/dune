Running `dune subst` should succeed in a directory containing just a `.opam` file.
Regression test for https://github.com/ocaml/dune/issues/11290

  $ cat > foo.opam << EOF
  > authors: [ "John Doe <john@doe.com>" ]
  > EOF

  $ dune subst
  File ".", line 1, characters 0-0:
  Error: There is no dune-project file in the current directory, please add one
  with a (name <name>) field in it.
  Hint: 'dune subst' must be executed from the root of the project.
  [1]
