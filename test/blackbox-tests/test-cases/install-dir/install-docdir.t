  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam docfile
  $ cat >dune <<EOF
  > (install
  >  (section doc)
  >  (files docfile))
  > EOF
  $ dune build @install
  $ mkdir install docdir
  $ dune install --dry-run --prefix ./install --docdir $PWD/docdir --display short 2>&1 | grep docdir
  Removing (if it exists) $TESTCASE_ROOT/docdir/foo/docfile
  Installing $TESTCASE_ROOT/docdir/foo/docfile
  Creating directory $TESTCASE_ROOT/docdir/foo
  Copying _build/install/default/doc/foo/docfile to $TESTCASE_ROOT/docdir/foo/docfile (executable: false)
  Removing (if it exists) $TESTCASE_ROOT/docdir/foo/odoc-config.sexp
  Installing $TESTCASE_ROOT/docdir/foo/odoc-config.sexp
  Creating directory $TESTCASE_ROOT/docdir/foo
  Copying _build/install/default/doc/foo/odoc-config.sexp to $TESTCASE_ROOT/docdir/foo/odoc-config.sexp (executable: false)
