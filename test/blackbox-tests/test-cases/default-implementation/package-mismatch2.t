A default implementation of a library must belong to the same package

  $ make_dune_project 2.6
  $ touch dummyfoo2.opam
  $ mkdir -p vlib impl
  $ touch vlib/vlib.mli impl/vlib.ml
  $ cat >vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (modules vlib)
  >  (virtual_modules vlib)
  >  (default_implementation def_i))
  > EOF
  $ cat >impl/dune <<EOF
  > (library
  >  (name def_i)
  >  (public_name dummyfoo2.bar)
  >  (implements vlib))
  > EOF
  $ dune build @install
  File "impl/dune", line 4, characters 13-17:
  4 |  (implements vlib))
                   ^^^^
  Error: Library "vlib" is private, it cannot be a dependency of a public
  library. You need to give "vlib" a public name.
  [1]
