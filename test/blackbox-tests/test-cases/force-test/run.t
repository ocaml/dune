  $ jbuilder clean --display short
  $ jbuilder runtest --display short
      ocamldep f.ml.d
        ocamlc .f.eobjs/f.{cmi,cmo,cmt}
      ocamlopt .f.eobjs/f.{cmx,o}
      ocamlopt f.exe
             f alias runtest
  Foo Bar
  $ jbuilder runtest --display short
  $ jbuilder runtest --force --display short
             f alias runtest
  Foo Bar
