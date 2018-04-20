This test define an installed "plop" with a "plop.ca-marche-pas"
sub-package which depend on a library that doesn't exist.

The build itself uses only "plop.ca-marche", which doesn't have this
problem. So jbuilder shouldn't crash because of "plop.ca-marche-pas"

We need ocamlfind to run this test

  $ jbuilder build @install --display short --only hello
  File "root/jbuild", line 5, characters 14-28:
  Error: Library "plop.ca-marche" not found.
  Hint: try: jbuilder external-lib-deps --missing --only-packages hello @install
        ocamlc root/.hello.objs/hello.{cmi,cmo,cmt}
      ocamlopt root/.hello.objs/hello.{cmx,o}
      ocamlopt root/hello.{a,cmxa}
      ocamlopt root/hello.cmxs
        ocamlc root/hello.cma
  [1]

  $ jbuilder build @install --display short --only pas-de-bol 2>&1 | sed 's/[^ "]*findlib-packages/.../'
  File "root/jbuild", line 11, characters 14-32:
  Error: Library "plop.ca-marche-pas" not found.
  Hint: try: jbuilder external-lib-deps --missing --only-packages pas-de-bol @install
      ocamldep root/a.ml.d
      ocamldep root/b.ml.d
        ocamlc root/.pas_de_bol.objs/pas_de_bol.{cmi,cmo,cmt}
      ocamlopt root/.pas_de_bol.objs/pas_de_bol.{cmx,o}
