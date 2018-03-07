  $ $JBUILDER build -j1 --root . --display short @all
      ocamldep test.ml.d
        ocamlc .test.eobjs/test.{cmi,cmo,cmt}
        ocamlc test.bc.o
           gcc static.bc
      ocamlopt .test.eobjs/test.{cmx,o}
      ocamlopt test.exe.o
           gcc static.exe
        ocamlc test.bc
        ocamlc test.bc.so
      ocamlopt test.exe
      ocamlopt test.so

  $ $JBUILDER build -j1 --root . --display quiet @runtest
        static alias runtest
  OK: ./static.bc
       dynamic alias runtest
  OK: ./dynamic.exe ./test.bc.so
        static alias runtest
  OK: ./static.exe
       dynamic alias runtest
  OK: ./dynamic.exe ./test.so
