Check that a type path referring to Stdlib.Format is resolved:

  $ dune build @doc-new
  $ grep unresolved _build/default/_doc_new/html/docs/local/odoctest/Odoctest/index.html
  [1]
