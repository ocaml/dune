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

  $ find $DUNE_CACHE_ROOT -type d | sort -u
  $TESTCASE_ROOT/dune-cache
  $TESTCASE_ROOT/dune-cache/files
  $TESTCASE_ROOT/dune-cache/files/v4
  $TESTCASE_ROOT/dune-cache/files/v4/11
  $TESTCASE_ROOT/dune-cache/files/v4/4d
  $TESTCASE_ROOT/dune-cache/files/v4/b8
  $TESTCASE_ROOT/dune-cache/meta
  $TESTCASE_ROOT/dune-cache/meta/v5
  $TESTCASE_ROOT/dune-cache/meta/v5/2f
  $TESTCASE_ROOT/dune-cache/meta/v5/39
  $TESTCASE_ROOT/dune-cache/meta/v5/4d
  $TESTCASE_ROOT/dune-cache/temp
  $TESTCASE_ROOT/dune-cache/values
  $TESTCASE_ROOT/dune-cache/values/v3

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
