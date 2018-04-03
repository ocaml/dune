  $ jbuilder clean --display short
  $ jbuilder exec --no-build ./foo.exe --display short
  Error: Program "./foo.exe" isn't built yet you need to buid it first or remove the --no-build option.
  [1]
  $ jbuilder exec ./foo.exe --display short
      ocamldep foo.ml.d
        ocamlc .foo.eobjs/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/foo.{cmx,o}
      ocamlopt foo.exe
  Foo
  $ jbuilder exec --dev ./foo.exe --display short
        ocamlc .foo.eobjs/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/foo.{cmx,o}
      ocamlopt foo.exe
  Foo
  $ jbuilder exec dunetestbar --no-build --display short
  Error: Program "dunetestbar" isn't built yet you need to buid it first or remove the --no-build option.
  [1]
  $ jbuilder exec dunetestbar --display short
      ocamldep bar.ml.d
        ocamlc .bar.eobjs/bar.{cmi,cmo,cmt}
      ocamlopt .bar.eobjs/bar.{cmx,o}
      ocamlopt bar.exe
  Bar
