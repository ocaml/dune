  $ $JBUILDER runtest --display short -j1 -p lib1 --debug-dependency-path
      ocamldep test1.ml.d
      ocamldep lib1.ml.d
        ocamlc .lib1.objs/lib1.{cmi,cmo,cmt}
        ocamlc .test1.eobjs/test1.{cmi,cmo,cmt}
      ocamlopt .lib1.objs/lib1.{cmx,o}
      ocamlopt .test1.eobjs/test1.{cmx,o}
      ocamlopt lib1.{a,cmxa}
      ocamlopt test1.exe
         test1 alias runtest
