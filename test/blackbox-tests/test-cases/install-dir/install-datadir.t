Honors `--datadir` for installed shared files.

  $ make_dune_project 2.0
  $ touch foo.opam datafile
  $ cat >dune <<EOF
  > (install
  >  (section share)
  >  (files datafile))
  > EOF
  $ dune build @install
  $ mkdir install datadir
  $ dune install --dry-run --prefix ./install --datadir $PWD/datadir --display short 2>&1 | grep datadir
  Creating directory $TESTCASE_ROOT/datadir/foo
  Removing (if it exists) $TESTCASE_ROOT/datadir/foo/datafile
  Installing $TESTCASE_ROOT/datadir/foo/datafile
  Copying _build/install/default/share/foo/datafile to $TESTCASE_ROOT/datadir/foo/datafile (executable: false)
