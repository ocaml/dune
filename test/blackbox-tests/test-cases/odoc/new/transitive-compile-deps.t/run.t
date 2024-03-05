This test checks that compilation dependencies are correct

  $ dune build @doc-new

There should be an expansion of S:

  $ ls _build/default/_doc_new/html/docs/local/odoctest3/Odoctest3/C/
  index.html
  module-type-S
