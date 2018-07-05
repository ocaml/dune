  $ dune runtest --display short
      ocamldep .bar.eobjs/bar.ml.d
      ocamldep .bar.eobjs/foo.ml.d
        ocamlc .bar.eobjs/foo.{cmi,cmo,cmt}
      ocamlopt .bar.eobjs/foo.{cmx,o}
      ocamlopt foo.exe
           foo alias runtest
  test foo
        ocamlc .bar.eobjs/bar.{cmi,cmo,cmt}
      ocamlopt .bar.eobjs/bar.{cmx,o}
      ocamlopt bar.exe
           bar bar.output
