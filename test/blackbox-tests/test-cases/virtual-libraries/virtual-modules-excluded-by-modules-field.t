Specifying a virtual module that isn't inside the (modules ..) field:

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ mkdir src
  $ cat > src/dune << EOF
  > (library
  >  (name foo)
  >  (wrapped false)
  >  (virtual_modules x)
  >  (modules y))
  > EOF

  $ touch src/x.mli

  $ cat > src/y.ml << EOF
  > module type F = X
  > EOF

  $ mkdir src/impl
  $ cat > src/impl/dune << EOF
  > (library
  >  (name impl)
  >  (implements foo))
  > EOF
  $ touch src/impl/x.ml

X is warned about:

  $ dune build --display short
      ocamldep impl/.impl.objs/x.impl.d
      ocamldep .foo.objs/y.impl.d
        ocamlc .foo.objs/byte/y.{cmi,cmo,cmt} (exit 2)
  File "y.ml", line 1, characters 16-17:
  1 | module type F = X
                      ^
  Error: Unbound module type X
  [1]
