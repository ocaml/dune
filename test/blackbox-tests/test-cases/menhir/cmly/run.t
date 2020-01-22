  $ dune build test.exe --display short --debug-dependency-path
      ocamllex lexer1.ml
      ocamldep .test.eobjs/lexer1.ml.d
      ocamldep .test.eobjs/test.ml.d
        menhir test_menhir1.{cmly,ml,mli}
      ocamldep .test.eobjs/test_menhir1.mli.d
      ocamldep .test.eobjs/test_menhir1.ml.d
        ocamlc .test.eobjs/byte/test_menhir1.{cmi,cmti}
      ocamlopt .test.eobjs/native/test_menhir1.{cmx,o}
        ocamlc .test.eobjs/byte/lexer1.{cmi,cmo,cmt}
        ocamlc .test.eobjs/byte/test.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/native/test.{cmx,o}
      ocamlopt .test.eobjs/native/lexer1.{cmx,o}
      ocamlopt test.exe
