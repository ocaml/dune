  $ $JBUILDER clean -j1 --root .
  $ $JBUILDER exec -b ./foo.exe -j1 --root .
      ocamldep foo.depends.ocamldep-output
        ocamlc foo.{cmi,cmo,cmt}
        ocamlc foo.exe
  Foo
