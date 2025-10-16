Create a cram test and try to run it with DUNE_BUILD_DIR set to an absolute
path

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > EOF

  $ cat >foo.t <<EOF
  >   $ echo "  $ echo bar" >bar.t
  >   $ dune runtest
  > EOF

  $ DUNE_BUILD_DIR=$PWD/tmp dune runtest --auto-promote
  File "foo.t", line 1, characters 0-0:
  Error: Files
  $TESTCASE_ROOT/tmp/default/foo.t
  and
  $TESTCASE_ROOT/tmp/default/foo.t.corrected
  differ.
  Promoting
    $TESTCASE_ROOT/tmp/default/foo.t.corrected
    to foo.t.
  [1]
  $ cat foo.t
    $ echo "  $ echo bar" >bar.t
    $ dune runtest
    Error: RPC server not running.
    [1]
