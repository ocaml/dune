  $ $JBUILDER build -j1 src/test.exe --root . --debug-dependency-path
      ocamllex src/lexer.ml
        menhir src/parser.{ml,mli}
        menhir src/tokens.{ml,mli}
      ocamldep src/test.depends.ocamldep-output
      ocamldep src/test.dependsi.ocamldep-output
        ocamlc src/tokens.{cmi,cmti}
      ocamlopt src/tokens.{cmx,o}
        ocamlc src/lexer.{cmi,cmo,cmt}
        ocamlc src/parser.{cmi,cmti}
      ocamlopt src/lexer.{cmx,o}
      ocamlopt src/parser.{cmx,o}
        ocamlc src/test.{cmi,cmo,cmt}
      ocamlopt src/test.{cmx,o}
      ocamlopt src/test.exe
