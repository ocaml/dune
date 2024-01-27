subst assumes that dune-project always exists:

  $ git init -q
  $ touch foo
  $ git add -A
  $ git commit -m "test" -q
  $ dune subst
  Error: dune-project: No such file or directory
  [1]
