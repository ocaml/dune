  $ jbuilder runtest --display short -p lib1 --debug-dependency-path
      ocamldep test1.ml.d
      ocamldep lib1.ml.d
        ocamlc .lib1.objs/lib1.{cmi,cmo,cmt}
      ocamlopt .lib1.objs/lib1.{cmx,$ext_obj}
      ocamlopt lib1.{$ext_lib,cmxa}
        ocamlc .test1.eobjs/test1.{cmi,cmo,cmt}
      ocamlopt .test1.eobjs/test1.{cmx,$ext_obj}
      ocamlopt test1.exe
         test1 alias runtest
