Tests for promoting with non existent reference
-----------------------------------------------
# Diffing should do as if it is an empty file

  $ file_status x-non-existent-empty
  x-non-existent-empty missing

  $ dune build @blah-non-existent-empty
  File "x-non-existent-empty", line 1, characters 0-0:
  Error: Files _build/default/x-non-existent-empty and _build/default/x.gen
  differ.
  [1]

  $ dune promote
  Promoting _build/default/x.gen to x-non-existent-empty.

  $ file_status x-non-existent-empty
  x-non-existent-empty exists

  $ file_status x-non-existent-non-empty
  x-non-existent-non-empty missing

  $ dune build @blah-non-existent-non-empty 2>&1 | dune_cmd subst '^.+/diff:' 'diff:'
  File "x-non-existent-non-empty", line 1, characters 0-0:
  --- x-non-existent-non-empty
  +++ y.gen
  @@ -0,0 +1 @@
  +foobar
  \ No newline at end of file
  [1]

  $ dune promote
  Promoting _build/default/y.gen to x-non-existent-non-empty.

  $ cat x-non-existent-non-empty
  foobar
