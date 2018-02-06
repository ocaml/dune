  $ $JBUILDER clean -j1 --display short --root .
  $ $JBUILDER runtest -j1 --display short --root .
      ocamldep f.ml.d
        ocamlc f.{cmi,cmo,cmt}
      ocamlopt f.{cmx,o}
      ocamlopt f.exe
             f alias runtest
  Foo Bar
  $ $JBUILDER runtest -j1 --display short --root .
  $ $JBUILDER runtest --force -j1 --display short --root .
             f alias runtest
  Foo Bar
