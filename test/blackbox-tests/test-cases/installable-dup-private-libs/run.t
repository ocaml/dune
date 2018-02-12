  $ $JBUILDER build @install -j1 --display short --root .
      ocamldep a1/a.ml.d
      ocamldep a2/a.ml.d
        ocamlc a1/.a.objs/a.{cmi,cmo,cmt}
        ocamlc a2/.a.objs/a.{cmi,cmo,cmt}
      ocamlopt a1/.a.objs/a.{cmx,o}
        ocamlc a1/a.cma
      ocamlopt a2/.a.objs/a.{cmx,o}
        ocamlc a2/a.cma
      ocamlopt a1/a.{a,cmxa}
      ocamlopt a2/a.{a,cmxa}
      ocamlopt a1/a.cmxs
      ocamlopt a2/a.cmxs
