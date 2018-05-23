  $ dune runtest --display short -p lib1 --debug-dependency-path
      ocamldep test1.ml.d
      ocamldep lib1.ml.d
        ocamlc .lib1.objs/lib1.{cmt,cmi,cmo}
      ocamlopt .lib1.objs/lib1.{cmx,o}
      ocamlopt lib1.{cmxa,a}
        ocamlc .test1.eobjs/test1.{cmo,cmt,cmi}
      ocamlopt .test1.eobjs/test1.{cmx,o}
      ocamlopt test1.exe
         test1 alias runtest
