  $ dune runtest --display short
      ocamldep .generated.eobjs/generated.ml.d
        ocamlc .generated.eobjs/generated.{cmi,cmo,cmt}
      ocamlopt .generated.eobjs/generated.{cmx,o}
      ocamlopt generated.exe
     generated generated.output
       patdiff (internal) (exit 1)
  (cd _build/default && /Users/rgrinberg/.opam/4.06.1/bin/patdiff -keep-whitespace -location-style omake -ascii generated.expected generated.output)
  ------ generated.expected
  ++++++ generated.output
  File "generated.expected", line 2, characters 0-1:
   |foo
  -|bar
  +|baz
      ocamldep .singular.eobjs/singular.ml.d
        ocamlc .singular.eobjs/singular.{cmi,cmo,cmt}
      ocamlopt .singular.eobjs/singular.{cmx,o}
      ocamlopt singular.exe
      singular alias runtest
  singular test
      ocamldep .expect_test.eobjs/expect_test.ml.d
      ocamldep .expect_test.eobjs/regular_test.ml.d
        ocamlc .expect_test.eobjs/expect_test.{cmi,cmo,cmt}
      ocamlopt .expect_test.eobjs/expect_test.{cmx,o}
      ocamlopt expect_test.exe
   expect_test expect_test.output
        ocamlc .expect_test.eobjs/regular_test.{cmi,cmo,cmt}
      ocamlopt .expect_test.eobjs/regular_test.{cmx,o}
      ocamlopt regular_test.exe
  regular_test alias runtest
  regular test
  [1]
