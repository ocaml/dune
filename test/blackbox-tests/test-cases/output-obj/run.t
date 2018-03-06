  $ rm -f /tmp/camlobj*
  $ $JBUILDER build -j1 --root . --display short
      ocamldep test.ml.d
        ocamlc .test.eobjs/test.{cmi,cmo,cmt}
        ocamlc test.bc.o
           gcc stub_byte
      ocamlopt .test.eobjs/test.{cmx,o}
      ocamlopt test.exe.o
           gcc stub_native

$ $JBUILDER build -j1 --root . --display quiet stub_native
$ $JBUILDER build -j1 --root . --display quiet stub_native
$ $JBUILDER build -j1 --root . --display quiet stub_byte
$ $JBUILDER build -j1 --root . --display quiet stub_byte
