  $ jbuilder clean -j1 --display short --root .
  $ jbuilder runtest -j1 --display short --root .
      ocamldep f.ml.d
        ocamlc .f.eobjs/f.{cmi,cmo,cmt}
      ocamlopt .f.eobjs/f.{cmx,o}
      ocamlopt f.exe
             f alias runtest
  Foo Bar
  $ jbuilder runtest -j1 --display short --root .
  $ jbuilder runtest --force -j1 --display short --root .
             f alias runtest
  Foo Bar
