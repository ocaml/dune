This test generates documentation for non-hidden modules only for a library:

  $ dune build @doc-new

 Hidden modules should be compiled
  $ find _build/default/_doc_new/odoc/foo -name '*.odoc' | sort -n
  _build/default/_doc_new/odoc/foo/foo.odoc
  _build/default/_doc_new/odoc/foo/foo__.odoc
  _build/default/_doc_new/odoc/foo/foo__Bar.odoc

 Hidden modules should not be linked
  $ find _build/default/_doc_new/odoc/foo -name '*.odocl' | sort -n
  _build/default/_doc_new/odoc/foo/foo.odocl

 We don't expect html for hidden modules
  $ find _build/default/_doc_new/html/docs/foo -name '*.html' | sort -n
  _build/default/_doc_new/html/docs/foo/Foo/index.html
  _build/default/_doc_new/html/docs/foo/index.html
