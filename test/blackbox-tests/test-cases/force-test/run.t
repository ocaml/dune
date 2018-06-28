  $ dune clean --display short
  $ dune runtest --display short
      ocamldep .f.eobjs/f.ml.d
        ocamlc .f.eobjs/f.{cmi,cmo,cmt}
      ocamlopt .f.eobjs/f.{cmx,o}
      ocamlopt f.exe
             f alias runtest
  Foo Bar
  $ dune runtest --display short
  $ dune runtest --force --display short
             f alias runtest
  Foo Bar
