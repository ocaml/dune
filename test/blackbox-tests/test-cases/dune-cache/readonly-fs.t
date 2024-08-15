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
  Error:
  mkdir($TESTCASE_ROOT/readonly/cache-dir): Permission denied
  [1]

Likewise, this should also happen if the location is set via XDG variables.

  $ unset DUNE_CACHE_ROOT
  $ export XDG_CACHE_HOME=$(pwd)/readonly/xdg-cache-dir

  $ dune build
  Error:
  mkdir($TESTCASE_ROOT/readonly/xdg-cache-dir): Permission denied
  [1]
