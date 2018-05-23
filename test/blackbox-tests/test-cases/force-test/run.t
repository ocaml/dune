  $ dune clean --display short
  $ dune runtest --display short
      ocamldep f.ml.d
        ocamlc .f.eobjs/f.{cmo,cmt,cmi}
      ocamlopt .f.eobjs/f.{cmx,o}
      ocamlopt f.exe
             f alias runtest
  Foo Bar
  $ dune runtest --display short
        ocamlc .f.eobjs/f.{cmi,cmo,cmt}
  $ dune runtest --force --display short
             f alias runtest
  Foo Bar
