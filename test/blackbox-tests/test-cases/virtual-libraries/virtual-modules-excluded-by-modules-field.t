Specifying a virtual module that isn't inside the (modules ..) field:

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (wrapped false)
  >  (virtual_modules x)
  >  (modules y))
  > EOF

  $ touch x.mli

  $ cat >y.ml <<EOF
  > module type F = X
  > EOF

  $ mkdir impl
  $ cat >impl/dune <<EOF
  > (library
  >  (name impl)
  >  (implements foo))
  > EOF
  $ touch impl/x.ml

  $ dune build --display short
      ocamldep impl/.impl.objs/x.impl.d
        ocamlc .foo.objs/byte/y.{cmi,cmo,cmt} (exit 2)
  File "y.ml", line 1, characters 16-17:
  1 | module type F = X
                      ^
  Error: Unbound module type X
  File "impl/dune", line 1, characters 0-40:
  1 | (library
  2 |  (name impl)
  3 |  (implements foo))
  Error: No rule found for .foo.objs/y.impl.all-deps
  [1]
