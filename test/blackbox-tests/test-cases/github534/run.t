  $ dune exec ./main.exe --display short
          echo main.ml
      ocamldep .main.eobjs/main.ml.d
        ocamlc .main.eobjs/byte/main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/native/main.{cmx,o}
      ocamlopt main.exe
  Hello World
