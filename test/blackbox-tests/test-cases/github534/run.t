  $ dune exec ./main.exe --display short
          echo main.ml
      ocamldep main.ml.d
        ocamlc .main.eobjs/main.{cmo,cmt,cmi}
      ocamlopt .main.eobjs/main.{cmx,o}
      ocamlopt main.exe
  Hello World
