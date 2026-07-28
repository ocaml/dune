Testing `dune show aliases` with absolute path arguments. Reproduces
#15103 (sub-issue of #12230).

  $ make_dune_project 3.25

  $ cat > dune <<EOF
  > (alias
  >  (name foo))
  > EOF

  $ mkdir subdir
  $ cat > subdir/dune <<EOF
  > (alias
  >  (name bar))
  > EOF

Relative paths from the workspace root work today. We filter to the
user-defined aliases so the test is not brittle to dune adding new
auto-generated aliases:

  $ dune show aliases . 2>&1 | grep -E '^(foo|bar)$'
  foo

CR-someday Alizter: an absolute path pointing at the same directory should
work identically to the relative form. Today it errors.

  $ dune show aliases $PWD
  Error: Directories outside of the project are not supported:
  $TESTCASE_ROOT

CR-someday Alizter: the same call from a subdirectory should also resolve.
Today the absolute path fails the same way. (--root is required because
INSIDE_DUNE disables workspace auto-detection.)

  $ (cd subdir && dune show aliases --root .. $PWD)
  Entering directory '..'
  Error: Directories outside of the project are not supported:
  $TESTCASE_ROOT/subdir
  Leaving directory '..'

CR-someday Alizter: absolute paths to subdirectories of the workspace
should be accepted.

  $ dune show aliases $PWD/subdir
  Error: Directories outside of the project are not supported:
  $TESTCASE_ROOT/subdir

Absolute paths that are genuinely outside the workspace must continue to
fail with a clean error. This behaviour must not regress.

  $ dune show aliases /tmp
  Error: Directories outside of the project are not supported: /tmp
