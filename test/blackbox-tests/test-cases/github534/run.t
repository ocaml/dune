  $ $JBUILDER exec ./main.exe --root . -j1 --display short
  File "jbuild", line 1, characters 0-124:
  Warning: Directory _build/default doesn't exist.
  File "jbuild", line 1, characters 0-89:
  Warning: Directory _build/default doesn't exist.
          echo main.ml
      ocamldep main.ml.d
        ocamlc .main.eobjs/main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/main.{cmx,o}
      ocamlopt main.exe
  Hello World
