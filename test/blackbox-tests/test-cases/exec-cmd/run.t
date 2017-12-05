  $ $JBUILDER clean -j1 --root .
  $ $JBUILDER exec --no-build ./foo.exe -j1 --root .
  Error: Program "./foo.exe" not found!
  [1]
  $ $JBUILDER exec ./foo.exe -j1 --root .
      ocamldep foo.depends.ocamldep-output
        ocamlc foo.{cmi,cmo,cmt}
        ocamlc foo.exe
  Foo
  $ $JBUILDER exec --dev ./foo.exe -j1 --root .
        ocamlc foo.{cmi,cmo,cmt}
        ocamlc foo.exe
  Foo
