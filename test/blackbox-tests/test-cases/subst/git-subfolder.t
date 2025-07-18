  $ . ../git-helpers.sh

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
