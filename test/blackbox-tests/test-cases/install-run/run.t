  $ $JBUILDER runtest -j1 --display short --root .
      ocamldep foo.ml.d
        ocamlc .foo.eobjs/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/foo.{cmx,o}
      ocamlopt foo.exe
      ocamldep bar.ml.d
        ocamlc .bar.eobjs/bar.{cmi,cmo,cmt}
      ocamlopt .bar.eobjs/bar.{cmx,o}
      ocamlopt bar.exe
           foo alias runtest
  bar
