  $ $JBUILDER build @install -j1 --display short --root .
      ocamldep a2/a.ml.d
        ocamlc a2/.a.objs/a.{cmi,cmo,cmt}
      ocamlopt a2/.a.objs/a.{cmx,o}
        ocamlc a2/a.cma
      ocamlopt a2/a.{a,cmxa}
      ocamlopt a2/a.cmxs
