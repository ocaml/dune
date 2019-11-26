  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam manfile
  $ cat >dune <<EOF
  > (install
  >  (section man)
  >  (files manfile))
  > EOF
  $ dune build @install
  $ mkdir install mandir
  $ dune install --dry-run --prefix ./install --mandir ./mandir 2>&1 | grep mandir
  Installing mandir/manfile
  Removing (if it exists) mandir/manfile
  Creating directory mandir
  Copying _build/install/default/man/manfile to mandir/manfile (executable: false)
