  $ dune build @install --display short
      ocamldep a1/a.ml.d
        ocamlc a1/.a.objs/a.{cmt,cmi,cmo}
      ocamlopt a1/.a.objs/a.{cmx,o}
      ocamlopt a1/a.{cmxa,a}
      ocamlopt a1/a.cmxs
      ocamldep a2/a.ml.d
        ocamlc a2/.a.objs/a.{cmt,cmi,cmo}
      ocamlopt a2/.a.objs/a.{cmx,o}
      ocamlopt a2/a.{cmxa,a}
      ocamlopt a2/a.cmxs
        ocamlc a1/a.cma
        ocamlc a2/a.cma
