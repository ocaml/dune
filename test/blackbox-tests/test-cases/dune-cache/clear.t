Test for the "dune cache clear" command.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/dune-cache

  $ cat >dune-project <<EOF
  > (lang dune 3.10)
  > EOF

  $ cat >dune <<EOF
  > (rule (with-stdout-to foo (progn)))
  > EOF

  $ dune build

  $ ls $DUNE_CACHE_ROOT | sort -u
  files
  meta
  temp
  values

  $ dune cache clear

  $ ! test -d $DUNE_CACHE_ROOT

Next let us add some extra directories/files and check that they are not deleted
by mistake.

  $ dune build

  $ mkdir -p $DUNE_CACHE_ROOT/extra; touch $DUNE_CACHE_ROOT/extra1 $DUNE_CACHE_ROOT/extra/extra2

  $ dune cache clear
  Error:
  rmdir($TESTCASE_ROOT/dune-cache): Directory not empty
  [1]

  $ find $DUNE_CACHE_ROOT -type f | sort -u
  $TESTCASE_ROOT/dune-cache/extra/extra2
  $TESTCASE_ROOT/dune-cache/extra1
