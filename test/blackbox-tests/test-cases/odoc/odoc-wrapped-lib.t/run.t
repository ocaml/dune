This test generates documentation for non-hidden modules only for a library:

  $ dune build @doc

 Hidden modules should be compiled
  $ find _build/default -name '*.odoc' | sort -n
  _build/default/_doc/_odoc/foo/foo/foo.odoc
  _build/default/_doc/_odoc/foo/foo/foo__.odoc
  _build/default/_doc/_odoc/foo/foo/foo__Bar.odoc
  _build/default/_doc/_odoc/foo/page-index.odoc

 Hidden modules should not be linked
  $ find _build/default -name '*.odocl' | sort -n
  _build/default/_doc/_odocls/foo/foo/foo.odocl
  _build/default/_doc/_odocls/foo/page-index.odocl

 We don't expect html for hidden modules
  $ find _build/default -name '*.html' | sort -n
  _build/default/_doc/_html/foo/foo/Foo/index.html
  _build/default/_doc/_html/foo/index.html
  _build/default/_doc/_html/index.html
