  $ jbuilder build test.exe .merlin --display short --debug-dependency-path
      ocamllex lexers/lexer1.ml
      ocamldep lexer1.ml.d
      ocamldep test.ml.d
      ocamldep dummy.ml.d
        ocamlc .foo.objs/dummy.{cmi,cmo,cmt}
      ocamlopt .foo.objs/dummy.{cmx,$ext_obj}
      ocamlopt foo.{$ext_lib,cmxa}
        ocamlc bar.$ext_obj
    ocamlmklib dllfoo_stubs.$ext_dll,libfoo_stubs.$ext_lib
        ocamlc .test.eobjs/lexer1.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/lexer1.{cmx,$ext_obj}
        ocamlc .test.eobjs/test.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/test.{cmx,$ext_obj}
      ocamlopt test.exe
  $ jbuilder build @bar-source --display short
  #line 1 "include/bar.h"
  int foo () {return 42;}
