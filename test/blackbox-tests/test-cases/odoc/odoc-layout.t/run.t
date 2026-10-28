Show where the documentation artifacts of each kind of library and package
pages are produced.

  $ dune build @doc @doc-private

TODO: move generated doc in _doc/_odoc/<parent-id>/...
  $ find . -name '*.odoc' | sort
  ./_build/default/_doc/_odoc/my_pkg/page-index.odoc
  ./_build/default/bar/.bar.objs/byte/bar.odoc
  ./_build/default/foo/.foo.objs/byte/foo.odoc
  ./_build/default/foo/.foo.objs/byte/foo__A.odoc
  ./_build/default/foo/.foo.objs/byte/foo__B.odoc
  ./_build/default/priv/.priv.objs/byte/priv.odoc
  ./_build/default/solo/.solo.objs/byte/solo.odoc

  $ find _build/default/_doc/_html -name '*.html' | sort
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/my_pkg/Bar/index.html
  _build/default/_doc/_html/my_pkg/Foo/A/index.html
  _build/default/_doc/_html/my_pkg/Foo/B/index.html
  _build/default/_doc/_html/my_pkg/Foo/index.html
  _build/default/_doc/_html/my_pkg/Priv/index.html
  _build/default/_doc/_html/my_pkg/index.html
  _build/default/_doc/_html/solo@e3b9de2fb703/Solo/index.html

  $ find . -name '*.odocl' | sort
  ./_build/default/_doc/_odocls/my_pkg/bar.odocl
  ./_build/default/_doc/_odocls/my_pkg/foo.odocl
  ./_build/default/_doc/_odocls/my_pkg/page-index.odocl
  ./_build/default/_doc/_odocls/my_pkg/priv.odocl
  ./_build/default/_doc/_odocls/solo@e3b9de2fb703/solo.odocl
