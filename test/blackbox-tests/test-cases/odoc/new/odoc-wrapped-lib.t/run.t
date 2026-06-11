This test generates documentation for non-hidden modules only for a library:

  $ html_dir=_build/default/_doc_new/html/docs/local/foo
  $ odoc_dir=_build/default/_doc_new/odoc/local/foo
  $ dune build \
  >   "$html_dir/Foo/index.html" \
  >   "$html_dir/index.html"

 Hidden modules should be compiled
  $ find "$odoc_dir" -name '*.odoc' | sort -n
  _build/default/_doc_new/odoc/local/foo/foo.odoc
  _build/default/_doc_new/odoc/local/foo/foo__.odoc
  _build/default/_doc_new/odoc/local/foo/foo__Bar.odoc

 Hidden modules should not be linked
  $ find "$odoc_dir" -name '*.odocl' | sort -n
  _build/default/_doc_new/odoc/local/foo/foo.odocl

 We don't expect html for hidden modules
  $ find "$html_dir" -name '*.html' | sort -n
  _build/default/_doc_new/html/docs/local/foo/Foo/index.html
  _build/default/_doc_new/html/docs/local/foo/index.html
