  $ jbuilder clean -j1 --display short --root .
  $ jbuilder exec --no-build ./foo.exe -j1 --display short --root .
  Error: Program "./foo.exe" isn't built yet you need to buid it first or remove the --no-build option.
  [1]
  $ jbuilder exec ./foo.exe -j1 --display short --root .
      ocamldep foo.ml.d
        ocamlc .foo.eobjs/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/foo.{cmx,o}
      ocamlopt foo.exe
  Foo
  $ jbuilder exec --dev ./foo.exe -j1 --display short --root .
        ocamlc .foo.eobjs/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/foo.{cmx,o}
      ocamlopt foo.exe
  Foo
  $ jbuilder exec dunetestbar --no-build -j1 --display short --root .
  Error: Program "dunetestbar" isn't built yet you need to buid it first or remove the --no-build option.
  [1]
  $ jbuilder exec dunetestbar -j1 --display short --root .
      ocamldep bar.ml.d
        ocamlc .bar.eobjs/bar.{cmi,cmo,cmt}
      ocamlopt .bar.eobjs/bar.{cmx,o}
      ocamlopt bar.exe
  Bar
