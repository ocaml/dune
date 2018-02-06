This test define an installed "plop" with a "plop.ca-marche-pas"
sub-package which depend on a library that doesn't exist.

The build itself uses only "plop.ca-marche", which doesn't have this
problem. So jbuilder shouldn't crash because of "plop.ca-marche-pas"

We need ocamlfind to run this test

  $ $JBUILDER build -j1 @install --display short --root . --only hello
        ocamlc hello.{cmi,cmo,cmt}
      ocamlopt hello.{cmx,o}
        ocamlc hello.cma
      ocamlopt hello.{a,cmxa}
      ocamlopt hello.cmxs

  $ $JBUILDER build -j1 @install --display short --root . --only pas-de-bol
  Error: External library "plop.ca-marche-pas" is unavailable.
  -> required by jbuild
  External library "plop.ca-marche-pas" is not available because it depends on the following libraries that are not available:
  - une-lib-qui-nexiste-pas -> not found
  Hint: try: jbuilder external-lib-deps --missing --root . --only-packages pas-de-bol @install
      ocamldep a.ml.d
      ocamldep b.ml.d
        ocamlc pas_de_bol.{cmi,cmo,cmt}
      ocamlopt pas_de_bol.{cmx,o}
  [1]
