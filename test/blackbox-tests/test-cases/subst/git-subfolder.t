Running `dune subst` in a subfolder of a Git repository should work.
Regression test for https://github.com/ocaml/dune/issues/11045

  $ git init --quiet
  $ dune init proj subfolder
  Entering directory 'subfolder'
  Success: initialized project component named subfolder
  Leaving directory 'subfolder'

  $ unset INSIDE_DUNE

`dune subst` requires at least one commit in the repository.

  $ git add -A && git commit --quiet --message "Initial commit"
  $ cd subfolder
  $ dune subst
  File ".", line 1, characters 0-0:
  Error: There is no dune-project file in the current directory, please add one
  with a (name <name>) field in it.
  Hint: 'dune subst' must be executed from the root of the project.
  [1]
