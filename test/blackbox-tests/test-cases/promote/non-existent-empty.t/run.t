Tests for promoting with non existent reference
-----------------------------------------------
# Diffing should do as if it is an empty file

  $ file_status x-non-existent-empty
  x-non-existent-empty missing

  $ dune build @blah-non-existent-empty

  $ dune promote

  $ file_status x-non-existent-empty
  x-non-existent-empty missing

  $ file_status x-non-existent-non-empty
  x-non-existent-non-empty missing

  $ dune build @blah-non-existent-non-empty 2>&1 | dune_cmd subst '^.+/diff:' 'diff:'
  File "x-non-existent-non-empty", line 1, characters 0-0:
  diff: x-non-existent-non-empty: No such file or directory
  [1]

  $ dune promote
  Promoting _build/default/y.gen to x-non-existent-non-empty.

  $ cat x-non-existent-non-empty
  foobar
