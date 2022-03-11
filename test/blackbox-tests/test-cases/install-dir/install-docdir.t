  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam docfile
  $ cat >dune <<EOF
  > (install
  >  (section doc)
  >  (files docfile))
  > EOF
  $ dune build @install
  $ mkdir install docdir
  $ dune install --dry-run --prefix ./install --docdir ./docdir 2>&1 | grep docdir
  Removing (if it exists) docdir/foo/docfile
  Installing docdir/foo/docfile
  Creating directory docdir/foo
  Copying _build/install/default/doc/foo/docfile to docdir/foo/docfile (executable: false)
