Check that a type path referring to Stdlib.Format is resolved:

  $ html=_build/default/_doc_new/html/docs/local/odoctest/Odoctest/index.html
  $ dune build "$html"
  $ grep unresolved "$html"
  [1]
