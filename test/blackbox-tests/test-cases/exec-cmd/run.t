  $ dune clean --display short
  $ dune exec --no-build ./foo.exe --display short
  Error: Program "./foo.exe" isn't built yet. You need to build it first or
  remove the --no-build option.
  [1]
  $ dune exec ./foo.exe --display short
      ocamldep .foo.eobjs/foo.ml.d
        ocamlc .foo.eobjs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/native/foo.{cmx,o}
      ocamlopt foo.exe
  Foo
  $ dune exec --profile release ./foo.exe --display short
        ocamlc .foo.eobjs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/native/foo.{cmx,o}
      ocamlopt foo.exe
  Foo
  $ dune exec dunetestbar --no-build --display short
  Error: Program "dunetestbar" isn't built yet. You need to build it first or
  remove the --no-build option.
  [1]
  $ dune exec dunetestbar --display short
      ocamldep .bar.eobjs/bar.ml.d
        ocamlc .bar.eobjs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .bar.eobjs/native/bar.{cmx,o}
      ocamlopt bar.exe
  Bar
