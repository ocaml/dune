  $ $JBUILDER build -j1 src/test.exe --root . --debug-dependency-path
      ocamllex src/lexer1.ml
      ocamllex src/lexer2.ml
        menhir src/test_base.{ml,mli}
        menhir src/test_menhir1.{ml,mli}
      ocamldep src/test.depends.ocamldep-output
      ocamldep src/test.dependsi.ocamldep-output
        ocamlc src/test_menhir1.{cmi,cmti}
        ocamlc src/test_base.{cmi,cmti}
      ocamlopt src/test_menhir1.{cmx,o}
        ocamlc src/lexer1.{cmi,cmo,cmt}
      ocamlopt src/test_base.{cmx,o}
        ocamlc src/lexer2.{cmi,cmo,cmt}
      ocamlopt src/lexer1.{cmx,o}
      ocamlopt src/lexer2.{cmx,o}
        ocamlc src/test.{cmi,cmo,cmt}
      ocamlopt src/test.{cmx,o}
      ocamlopt src/test.exe
