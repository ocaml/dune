  $ $JBUILDER build -j1 test.exe --root . --debug-dependency-path
      ocamllex lexer1.ml
      ocamllex lexer2.ml
        menhir test_base.{ml,mli}
        menhir test_menhir1.{ml,mli}
      ocamldep test.depends.ocamldep-output
      ocamldep test.dependsi.ocamldep-output
        ocamlc test_menhir1.{cmi,cmti}
        ocamlc test_base.{cmi,cmti}
      ocamlopt test_menhir1.{cmx,o}
        ocamlc lexer1.{cmi,cmo,cmt}
      ocamlopt test_base.{cmx,o}
        ocamlc lexer2.{cmi,cmo,cmt}
      ocamlopt lexer1.{cmx,o}
      ocamlopt lexer2.{cmx,o}
        ocamlc test.{cmi,cmo,cmt}
      ocamlopt test.{cmx,o}
      ocamlopt test.exe
