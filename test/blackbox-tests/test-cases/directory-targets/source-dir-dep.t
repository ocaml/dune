Depend on a source directory.

Currently, this feature isn't working. It's only possible to depend on
directories that are a target of a rule.

  $ make_directory_targets_project 3.0

  $ mkdir foo
  $ touch foo/{x,y,z}

  $ cat >dune <<EOF
  > (rule
  >  (deps foo)
  >  (target bar)
  >  (action (bash "ls -f %{deps} > %{target}")))
  > EOF

  $ dune build ./bar
  File "dune", lines 1-4, characters 0-77:
  1 | (rule
  2 |  (deps foo)
  3 |  (target bar)
  4 |  (action (bash "ls -f %{deps} > %{target}")))
  Error: No rule found for foo
  [1]
