  $ $JBUILDER build -j1 test.exe .merlin --root . --debug-dependency-path
      ocamllex lexers/lexer1.ml
      ocamldep test.depends.ocamldep-output
      ocamldep foo.depends.ocamldep-output
        ocamlc lexer1.{cmi,cmo,cmt}
        ocamlc bar.o
      ocamlopt foo.{a,cmxa}
      ocamlopt lexer1.{cmx,o}
        ocamlc test.{cmi,cmo,cmt}
    ocamlmklib dllfoo_stubs.so,libfoo_stubs.a
      ocamlopt test.{cmx,o}
      ocamlopt test.exe
