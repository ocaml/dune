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
  Multiple rules generated for _build/.aliases/default/runtest-decee903a70d612bb35f9747eb00e4b2:
  - dune:2
  - dune:2
  [1]
  $ dune runtest --root generated --display short
  Entering directory 'generated'
      ocamldep .generated.eobjs/generated.ml.d
        ocamlc .generated.eobjs/generated.{cmi,cmo,cmt}
      ocamlopt .generated.eobjs/generated.{cmx,o}
      ocamlopt generated.exe
     generated generated.output
  File "generated.expected", line 1, characters 0-0:
  Files _build/default/generated.expected and _build/default/generated.output differ.
  [1]
