  $ dune runtest --root singular --display short
  Entering directory 'singular'
      ocamldep .singular.eobjs/singular.ml.d
        ocamlc .singular.eobjs/singular.{cmi,cmo,cmt}
      ocamlopt .singular.eobjs/singular.{cmx,o}
      ocamlopt singular.exe
      singular alias runtest
  singular test

  $ dune runtest --root plural --display short
  Entering directory 'plural'
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
  $ dune runtest --diff-command false --root generated --display short
  Entering directory 'generated'
      ocamldep .generated.eobjs/generated.ml.d
        ocamlc .generated.eobjs/generated.{cmi,cmo,cmt}
      ocamlopt .generated.eobjs/generated.{cmx,o}
      ocamlopt generated.exe
     generated generated.output
            sh (internal) (exit 1)
  (cd _build/default && /bin/sh -c 'false generated.expected generated.output')
  [1]
