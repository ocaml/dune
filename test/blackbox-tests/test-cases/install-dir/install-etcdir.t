  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam configfile
  $ cat >dune <<EOF
  > (install
  >  (section etc)
  >  (files configfile))
  > EOF
  $ dune build @install
  $ mkdir install etcdir
  $ dune install --dry-run --prefix ./install --etcdir $PWD/etcdir --display short 2>&1 | grep etcdir
  Removing (if it exists) $TESTCASE_ROOT/etcdir/foo/configfile
  Installing $TESTCASE_ROOT/etcdir/foo/configfile
  Creating directory $TESTCASE_ROOT/etcdir/foo
  Copying _build/install/default/etc/foo/configfile to $TESTCASE_ROOT/etcdir/foo/configfile (executable: false)
