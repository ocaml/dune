  $ jbuilder exec ./main.exe --root . -j1 --display short
          echo main.ml
      ocamldep main.ml.d
        ocamlc .main.eobjs/main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/main.{cmx,o}
      ocamlopt main.exe
  Hello World
