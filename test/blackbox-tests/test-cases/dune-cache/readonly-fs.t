The cache can't be written if the location to where it is supposed to be
written can't be written to.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > EOF
  $ cat >dune <<EOF
  > (rule (with-stdout-to foo (progn)))
  > EOF

Create a directory in which we can't write to and use this as the location
where Dune is supposed to store the cache:

  $ mkdir readonly
  $ chmod a-w readonly
  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$(pwd)/readonly/cache-dir

  $ dune build
  Warning: Cache directories could not be created: Permission denied; disabling
  cache
  Hint: Make sure the directory
  $TESTCASE_ROOT/readonly/cache-dir/temp
  can be created

Likewise, this should also happen if the location is set via XDG variables.

  $ unset DUNE_CACHE_ROOT
  $ export XDG_CACHE_HOME=$(pwd)/readonly/xdg-cache-dir
  $ export DUNE_CONFIG__SKIP_LINE_BREAK=enabled

  $ dune build 2>&1 | sed 's/created: .*;/created: $REASON:/'
  Warning: Cache directories could not be created: $REASON: disabling cache
  Hint: Make sure the directory $TESTCASE_ROOT/readonly/xdg-cache-dir/dune/db/temp can be created

  $ export HOME=/homeless-shelter
  $ unset XDG_CACHE_HOME
  $ dune build 2>&1 | sed 's/created: .*;/created: $REASON:/'
  Warning: Cache directories could not be created: $REASON: disabling cache
  Hint: Make sure the directory /homeless-shelter/.cache/dune/db/temp can be created
