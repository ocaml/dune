The build-path-prefix-map substitutes any dynamic path in the output of
commands, to help with reproducibility:

  $ PWD=$(pwd)
  $ echo $PWD
  $TESTCASE_ROOT
  $ echo /nest/$PWD/sub/dir
  /nest/$TESTCASE_ROOT/sub/dir
  $ echo "[\"$PWD\",\"$PWD\"]"
  ["$TESTCASE_ROOT","$TESTCASE_ROOT"]

Besides the current `$TESTCASE_ROOT` directory, an empty `$TMPDIR` is available:

  $ ls -a $TMPDIR
  .
  ..
  $ echo $TMPDIR
  $TMPDIR
  $ echo $TMPDIR/sub/dir
  $TMPDIR/sub/dir
  $ echo The tempdir is at:$TMPDIR:
  The tempdir is at:$TMPDIR:

And the dune workspace root is replaced by `/workspace_root`:

  $ dirname $(pwd)
  /workspace_root/default/test/blackbox-tests/test-cases

The environment variable `$BUILD_PATH_PREFIX_MAP` can be extended with new
entries, e.g. the user `$HOME` directory:

  $ export BUILD_PATH_PREFIX_MAP="HOME=$HOME:$BUILD_PATH_PREFIX_MAP"
  $ echo $HOME
  HOME

Spaces in paths are supported:

  $ SPACED="path/contains spaces/.but/it's not an"
  $ echo /this/$SPACED/issue
  /this/path/contains spaces/.but/it's not an/issue
  $ export BUILD_PATH_PREFIX_MAP="\$SPACED=$SPACED:$BUILD_PATH_PREFIX_MAP"
  $ echo /this/$SPACED/issue
  /this/$SPACED/issue

Right-most entries are preferred:

  $ SUBDIR="$(pwd)/sub"
  $ export BUILD_PATH_PREFIX_MAP="\$LEFT=$SUBDIR:$BUILD_PATH_PREFIX_MAP"
  $ echo $SUBDIR
  $TESTCASE_ROOT/sub
  $ export BUILD_PATH_PREFIX_MAP="$BUILD_PATH_PREFIX_MAP:\$RIGHT=$SUBDIR"
  $ echo $SUBDIR
  $RIGHT

Inspecting the `$BUILD_PATH_PREFIX_MAP` should show no dynamic path as they are
all replaced by their binding:

  $ echo $BUILD_PATH_PREFIX_MAP
  $LEFT=$RIGHT:$SPACED=$SPACED:HOME=HOME:/workspace_root=/workspace_root:$TESTCASE_ROOT=$TESTCASE_ROOT:$TMPDIR=$TMPDIR:$RIGHT=$RIGHT
