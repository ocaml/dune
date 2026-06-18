Create a cram test and try to run it with DUNE_BUILD_DIR set to an absolute
path

  $ make_dune_project 3.5

  $ cat >foo.t <<'EOF'
  >   $ echo "  $ echo bar" >bar.t
  >   $ if [ -e "$DUNE_BUILD_DIR/.rpc" ]; then
  >   >   echo ".rpc exists"
  >   > else
  >   >   echo ".rpc missing"
  >   > fi
  >   .rpc missing
  >   $ env -u DUNE_RPC -u DUNE_BUILD_DIR dune runtest
  > EOF

  $ DUNE_BUILD_DIR=$PWD/tmp dune runtest --auto-promote
  File "foo.t", line 1, characters 0-0:
  --- foo.t
  +++ foo.t.corrected
  @@ -6,3 +6,10 @@
     > fi
     .rpc missing
     $ env -u DUNE_RPC -u DUNE_BUILD_DIR dune runtest
  +  File "bar.t", line 1, characters 0-0:
  +  --- bar.t
  +  +++ bar.t.corrected
  +  @@ -1 +1,2 @@
  +     $ echo bar
  +  +  bar
  +  [1]
  Promoting
    $TESTCASE_ROOT/tmp/default/foo.t.corrected
    to foo.t.
  [1]
  $ cat foo.t
    $ echo "  $ echo bar" >bar.t
    $ if [ -e "$DUNE_BUILD_DIR/.rpc" ]; then
    >   echo ".rpc exists"
    > else
    >   echo ".rpc missing"
    > fi
    .rpc missing
    $ env -u DUNE_RPC -u DUNE_BUILD_DIR dune runtest
    File "bar.t", line 1, characters 0-0:
    --- bar.t
    +++ bar.t.corrected
    @@ -1 +1,2 @@
       $ echo bar
    +  bar
    [1]
