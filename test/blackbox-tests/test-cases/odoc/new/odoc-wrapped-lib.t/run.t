This test generates documentation for non-hidden modules only for a library:

  $ dune build @doc-new

 Hidden modules should be compiled
  $ find _build/default/_doc_new/odoc/local/foo -name '*.odoc' | sort -n
  _build/default/_doc_new/odoc/local/foo/foo.odoc
  _build/default/_doc_new/odoc/local/foo/foo__.odoc
  _build/default/_doc_new/odoc/local/foo/foo__Bar.odoc

 Hidden modules should not be linked
  $ find _build/default/_doc_new/odoc/local/foo -name '*.odocl' | sort -n
  _build/default/_doc_new/odoc/local/foo/foo.odocl

 We don't expect html for hidden modules
  $ find _build/default/_doc_new/html/docs/local/foo -name '*.html' | sort -n
  _build/default/_doc_new/html/docs/local/foo/Foo/index.html
  _build/default/_doc_new/html/docs/local/foo/index.html
