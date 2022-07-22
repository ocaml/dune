  $ cat >dune-project <<EOF
  > (lang dune 3.3)
  > EOF

  $ mkdir vlib impl
  $ touch vlib/foo.mli
  $ cat >vlib/dune <<EOF
  > (library
  >  (name foo)
  >  (virtual_modules foo)
  >  (wrapped false))
  > EOF

  $ touch impl/foo.ml impl/new_public_module.ml
  $ cat >impl/dune <<EOF
  > (library
  >  (name bar)
  >  (implements foo))
  > EOF

  $ dune build
