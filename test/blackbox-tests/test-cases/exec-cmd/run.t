  $ $JBUILDER clean -j1 --root .
  $ $JBUILDER exec --no-build ./foo.exe -j1 --root .
  Error: Program "./foo.exe" isn't built yet you need to buid it first or remove the --no-build option.
  [1]
  $ $JBUILDER exec ./foo.exe -j1 --root .
      ocamldep foo.depends.ocamldep-output
        ocamlc foo.{cmi,cmo,cmt}
      ocamlopt foo.{cmx,o}
      ocamlopt foo.exe
  Foo
  $ $JBUILDER exec --dev ./foo.exe -j1 --root .
        ocamlc foo.{cmi,cmo,cmt}
      ocamlopt foo.{cmx,o}
      ocamlopt foo.exe
  Foo
  $ $JBUILDER exec bar --no-build -j1 --root .
  Error: Program "bar" isn't built yet you need to buid it first or remove the --no-build option.
  [1]
  $ $JBUILDER exec bar -j1 --root .
      ocamldep bar.depends.ocamldep-output
        ocamlc bar.{cmi,cmo,cmt}
      ocamlopt bar.{cmx,o}
      ocamlopt bar.exe
  Bar
