Test that includes vlib and implementations all in the same folder.

  $ echo "(lang dune 3.6)" > dune-project

  $ touch empty.mli
  $ cat >dune <<EOF
  > (library
  >  (name impl_one)
  >  (implements vlib))
  > (library
  >  (name vlib)
  >  (virtual_modules empty))
  > EOF
  $ dune build
  File "dune", line 1, characters 0-45:
  1 | (library
  2 |  (name impl_one)
  3 |  (implements vlib))
  Error: Virtual library and its implementation(s) cannot be defined in the
  same directory
  [1]
