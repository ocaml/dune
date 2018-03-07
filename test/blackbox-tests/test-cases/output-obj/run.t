  $ $JBUILDER build -j1 --root . --display short @all
      ocamldep test.ml.d
        ocamlc .test.eobjs/test.{cmi,cmo,cmt}
        ocamlc test.bc.o
           gcc prog.bc.exe
      ocamlopt .test.eobjs/test.{cmx,o}
      ocamlopt test.exe.o
           gcc prog.exe
        ocamlc test.bc
        ocamlc test.bc.so
      ocamlopt test.exe
      ocamlopt test.so

  $ $JBUILDER build -j1 --root . --display short @runtest
          prog alias runtest
  Hello, world from manually linked native program
          prog alias runtest
  Hello, world from manually linked byte program
