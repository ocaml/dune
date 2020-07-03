  $ dune runtest --display short
      ocamldep .foo.objs/foo.ml.d
      ocamldep .bar.objs/bar.ml.d
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc .bar.objs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .bar.objs/native/bar.{cmx,o}
        ocamlc bar.cma
      ocamlopt bar.{a,cmxa}
      ocamlopt bar.cmxs
      ocamlopt .foo.objs/native/foo.{cmx,o}
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
     ocamlfind test.exe
          test alias runtest
  42 42
