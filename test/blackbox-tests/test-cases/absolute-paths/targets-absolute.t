Testing `dune show targets` with absolute path arguments. Reproduces
#15103 (sub-issue of #12230).

  $ make_dune_project 3.25

  $ cat > dune <<EOF
  > (library
  >  (name simple))
  > EOF

  $ mkdir subdir
  $ cat > subdir/dune <<EOF
  > (library
  >  (name subdir_lib))
  > EOF

Relative paths from the workspace root work today:

  $ dune show targets .
  dune
  dune-project
  simple.a
  simple.cma
  simple.cmxa
  simple.cmxs
  simple.ml-gen

CR-someday Alizter: an absolute path pointing at the same directory should
work identically to the relative form. Today it errors.

  $ dune show targets $PWD
  Error: Directories outside of the project are not supported:
  $TESTCASE_ROOT

CR-someday Alizter: the same call from a subdirectory should also resolve.
Today the absolute path fails the same way. (--root is required because
INSIDE_DUNE disables workspace auto-detection.)

  $ (cd subdir && dune show targets --root .. $PWD)
  Entering directory '..'
  Error: Directories outside of the project are not supported:
  $TESTCASE_ROOT/subdir
  Leaving directory '..'

CR-someday Alizter: absolute paths to subdirectories of the workspace
should be accepted.

  $ dune show targets $PWD/subdir
  Error: Directories outside of the project are not supported:
  $TESTCASE_ROOT/subdir

Absolute paths that are genuinely outside the workspace must continue to
fail with a clean error. This behaviour must not regress.

  $ dune show targets /tmp
  Error: Directories outside of the project are not supported: /tmp
