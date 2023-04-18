@all builds private libs

  $ dune build --display short @all
      ocamldep .bar.objs/bar.impl.d
        ocamlc .bar.objs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .bar.objs/native/bar.{cmx,o}
        ocamlc bar.cma
      ocamlopt bar.{a,cmxa}
      ocamlopt bar.cmxs
