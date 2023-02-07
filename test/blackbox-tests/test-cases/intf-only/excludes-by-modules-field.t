Specifying a module without implementation that isn't inside the (modules ..)
field

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (wrapped false)
  >  (modules_without_implementation x)
  >  (modules y))
  > EOF

  $ touch x.mli

  $ cat >y.ml <<EOF
  > module type F = X
  > EOF

  $ dune build --display short
        ocamlc .foo.objs/byte/y.{cmi,cmo,cmt} (exit 2)
  File "y.ml", line 1, characters 16-17:
  1 | module type F = X
                      ^
  Error: Unbound module type X
  [1]
