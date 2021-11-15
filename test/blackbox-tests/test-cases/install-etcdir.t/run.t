  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam configfile
  $ cat >dune <<EOF
  > (install
  >  (section etc)
  >  (files configfile))
  > EOF
  $ dune build @install
  $ mkdir install etcdir
  $ dune install --dry-run --prefix ./install --etcdir ./etcdir 2>&1 | grep etcdir
  Removing (if it exists) etcdir/foo/configfile
  Installing etcdir/foo/configfile
  Creating directory etcdir/foo
  Copying _build/install/default/etc/foo/configfile to etcdir/foo/configfile (executable: false)
