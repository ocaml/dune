  $ dune runtest --display short
      ocamldep .singular.eobjs/singular.ml.d
        ocamlc .singular.eobjs/singular.{cmi,cmo,cmt}
      ocamlopt .singular.eobjs/singular.{cmx,o}
      ocamlopt singular.exe
      singular alias runtest
  singular test
      ocamldep .expect_test.eobjs/expect_test.ml.d
      ocamldep .expect_test.eobjs/regular_test.ml.d
        ocamlc .expect_test.eobjs/regular_test.{cmi,cmo,cmt}
      ocamlopt .expect_test.eobjs/regular_test.{cmx,o}
      ocamlopt regular_test.exe
  regular_test alias runtest
  regular test
        ocamlc .expect_test.eobjs/expect_test.{cmi,cmo,cmt}
      ocamlopt .expect_test.eobjs/expect_test.{cmx,o}
      ocamlopt expect_test.exe
   expect_test expect_test.output
