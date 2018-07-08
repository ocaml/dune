  $ dune build --display short @all
      ocamldep .test.eobjs/test.ml.d
        ocamlc .test.eobjs/test.{cmi,cmo,cmt}
        ocamlc test.bc.o
           gcc static.bc
      ocamlopt .test.eobjs/test.{cmx,o}
      ocamlopt test.exe.o
           gcc static.exe
        ocamlc test.bc
        ocamlc test.bc.so
      ocamlopt test.exe
      ocamlopt test$ext_dll

  $ dune build @runtest
       dynamic alias runtest
  OK: ./dynamic.exe ./test.bc.so
        static alias runtest
  OK: ./static.bc
        static alias runtest
  OK: ./static.exe
       dynamic alias runtest
  OK: ./dynamic.exe ./test$ext_dll
