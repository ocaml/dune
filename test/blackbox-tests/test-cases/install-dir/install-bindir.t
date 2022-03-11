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
  $ dune install --dry-run --prefix ./install --bindir ./bindir --sbindir ./sbindir 2>&1 | grep bindir
  Removing (if it exists) bindir/user
  Installing bindir/user
  Creating directory bindir
  Copying _build/install/default/bin/user to bindir/user (executable: true)
  Removing (if it exists) sbindir/admin.exe
  Installing sbindir/admin.exe
  Creating directory sbindir
  Copying _build/install/default/sbin/admin.exe to sbindir/admin.exe (executable: true)
