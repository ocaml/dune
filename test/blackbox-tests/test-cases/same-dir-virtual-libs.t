Test that includes vlib and implementations all in the same folder.

  $ echo "(lang dune 3.6)" > dune-project

  $ touch empty.mli
  $ cat >dune <<EOF
  > (library
  >  (name impl_one)
  >  (implements vlib))
  > (library
  >  (name impl_two)
  >  (implements vlib))
  > (library
  >  (name vlib)
  >  (virtual_modules empty))
  > EOF

  $ dune build
