A default implementation of a library must belong to the same package

  $ echo "(lang dune 2.6)" > dune-project
  $ touch dummyfoo1.opam
  $ touch dummyfoo2.opam
  $ mkdir vlib impl
  $ touch vlib/vlib.mli impl/vlib.ml
  $ cat >vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (modules vlib)
  >  (public_name dummyfoo1.bar)
  >  (virtual_modules vlib)
  >  (default_implementation def_i))
  > EOF
  $ cat >impl/dune <<EOF
  > (library
  >  (name def_i)
  >  (public_name dummyfoo2.bar)
  >  (implements dummyfoo1.bar))
  > EOF
  $ dune build @install
  File "vlib/dune", line 6, characters 25-30:
  6 |  (default_implementation def_i))
                               ^^^^^
  Error: default implementation belongs to package dummyfoo2 while virtual
  library belongs to package dummyfoo1. This is impossible.
  -> required by _build/default/dummyfoo1.dune-package
  -> required by _build/install/default/lib/dummyfoo1/dune-package
  -> required by _build/default/dummyfoo1.install
  -> required by alias install
  [1]
