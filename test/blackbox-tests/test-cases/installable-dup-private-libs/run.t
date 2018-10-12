  $ dune build @install --display short
      ocamldep a1/.a.objs/a.ml.d
        ocamlc a1/.a.objs/a.{cmi,cmo,cmt}
        ocamlc a1/a.cma
      ocamldep a2/.a.objs/a.ml.d
        ocamlc a2/.a.objs/a.{cmi,cmo,cmt}
        ocamlc a2/a.cma
      ocamlopt a1/.a.objs/a.{cmx,o}
      ocamlopt a1/a.{a,cmxa}
      ocamlopt a1/a.cmxs
      ocamlopt a2/.a.objs/a.{cmx,o}
      ocamlopt a2/a.{a,cmxa}
      ocamlopt a2/a.cmxs
