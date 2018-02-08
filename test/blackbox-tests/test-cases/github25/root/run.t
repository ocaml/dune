This test define an installed "plop" with a "plop.ca-marche-pas"
sub-package which depend on a library that doesn't exist.

The build itself uses only "plop.ca-marche", which doesn't have this
problem. So jbuilder shouldn't crash because of "plop.ca-marche-pas"

We need ocamlfind to run this test

  $ $JBUILDER build -j1 @install --display short --root . --only hello
        ocamlc .hello.objs/hello.{cmi,cmo,cmt}
      ocamlopt .hello.objs/hello.{cmx,o}
        ocamlc hello.cma
      ocamlopt hello.{a,cmxa}
      ocamlopt hello.cmxs

  $ $JBUILDER build -j1 @install --display short --root . --only pas-de-bol
  Error: External library "une-lib-qui-nexiste-pas" not found.
  -> required by library "plop.ca-marche-pas"
  -> required by jbuild
  Hint: try: jbuilder external-lib-deps --missing --root . --only-packages pas-de-bol @install
      ocamldep a.ml.d
      ocamldep b.ml.d
        ocamlc .pas_de_bol.objs/pas_de_bol.{cmi,cmo,cmt}
      ocamlopt .pas_de_bol.objs/pas_de_bol.{cmx,o}
  [1]
