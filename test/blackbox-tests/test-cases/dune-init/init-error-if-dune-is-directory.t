Report an error if `dune init ...` would create a "dune" file in a location
which already has a "dune" directory.

  $ mkdir dune

  $ dune init exe foo
  Error:
  "$TESTCASE_ROOT/dune"
  already exists and is a directory
  [1]

  $ dune init lib foo
  Error:
  "$TESTCASE_ROOT/dune"
  already exists and is a directory
  [1]

  $ dune init test foo
  Error:
  "$TESTCASE_ROOT/dune"
  already exists and is a directory
  [1]
