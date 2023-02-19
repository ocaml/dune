  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam user.ml admin.ml
  $ cat >dune <<EOF
  > (executable
  >  (public_name user)
  >  (modules user))
  > (executable
  >  (name admin)
  >  (modules admin))
  > (install
  >  (section sbin)
  >  (files admin.exe))
  > EOF
  $ dune build @install
  $ mkdir install bindir sbindir
  $ dune install --dry-run --prefix ./install --bindir $PWD/bindir --sbindir $PWD/sbindir --display short 2>&1 | grep bindir
  Removing (if it exists) $TESTCASE_ROOT/bindir/user
  Installing $TESTCASE_ROOT/bindir/user
  Creating directory $TESTCASE_ROOT/bindir
  Copying _build/install/default/bin/user to $TESTCASE_ROOT/bindir/user (executable: true)
  Removing (if it exists) $TESTCASE_ROOT/sbindir/admin.exe
  Installing $TESTCASE_ROOT/sbindir/admin.exe
  Creating directory $TESTCASE_ROOT/sbindir
  Copying _build/install/default/sbin/admin.exe to $TESTCASE_ROOT/sbindir/admin.exe (executable: true)
