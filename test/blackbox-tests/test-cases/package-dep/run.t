  $ $JBUILDER runtest -j1 --display short --root .
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
      ocamlopt .foo.objs/foo.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
        ocamlc foo.cma
  package foo
