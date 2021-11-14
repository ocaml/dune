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
  Removing (if it exists, even if it is an empty directory) mandir/manfile
  Installing mandir/manfile
  Creating directory mandir
  Copying _build/install/default/man/manfile to mandir/manfile (executable: false)
