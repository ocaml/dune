  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam datafile
  $ cat >dune <<EOF
  > (install
  >  (section share)
  >  (files datafile))
  > EOF
  $ dune build @install
  $ mkdir install datadir
  $ dune install --dry-run --prefix ./install --datadir ./datadir 2>&1 | grep datadir
  Removing (if it exists) datadir/foo/datafile
  Installing datadir/foo/datafile
  Creating directory datadir/foo
  Copying _build/install/default/share/foo/datafile to datadir/foo/datafile (executable: false)
