  $ $JBUILDER clean -j1 --root .
  $ $JBUILDER runtest -j1 --root .
      ocamldep f.depends.ocamldep-output
        ocamlc f.{cmi,cmo,cmt}
      ocamlopt f.{cmx,o}
      ocamlopt f.exe
             f alias runtest
  Foo Bar
  $ $JBUILDER runtest -j1 --root .
  $ $JBUILDER runtest --force -j1 --root .
             f alias runtest
  Foo Bar
