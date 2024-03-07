  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam manfile
  $ cat >dune <<EOF
  > (install
  >  (section man)
  >  (files manfile))
  > EOF
  $ dune build @install
  $ mkdir install mandir
  $ dune install --dry-run --prefix ./install --mandir $PWD/mandir --display short 2>&1 | grep mandir
  Removing (if it exists) $TESTCASE_ROOT/mandir/manfile
  Installing $TESTCASE_ROOT/mandir/manfile
  Creating directory $TESTCASE_ROOT/mandir
  Copying _build/install/default/man/manfile to $TESTCASE_ROOT/mandir/manfile (executable: false)
