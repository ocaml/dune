  $ dune runtest --display short
      ocamldep bar.ml.d
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo.{cmt,cmi,cmo}
        ocamlc .bar.objs/bar.{cmt,cmi,cmo}
        ocamlc bar.cma
      ocamlopt .foo.objs/foo.{cmx,o}
      ocamlopt .bar.objs/bar.{cmx,o}
      ocamlopt bar.{cmxa,a}
      ocamlopt bar.cmxs
        ocamlc foo.cma
      ocamlopt foo.{cmxa,a}
      ocamlopt foo.cmxs
     ocamlfind test.exe
          test alias runtest
  42 42
