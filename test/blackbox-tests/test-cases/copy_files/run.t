  $ $JBUILDER build -j1 test.exe .merlin --root . --debug-dependency-path
      ocamllex lexers/lexer1.ml
      ocamldep test.depends.ocamldep-output
        ocamlc lexer1.{cmi,cmo,cmt}
      ocamlopt lexer1.{cmx,o}
        ocamlc test.{cmi,cmo,cmt}
      ocamlopt test.{cmx,o}
      ocamlopt test.exe
