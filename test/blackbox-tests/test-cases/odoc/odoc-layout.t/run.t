Show where the documentation artifacts of each kind of library and package
pages are produced.

  $ dune build @doc @doc-private

  $ find . -name '*.odoc' | sort
  ./_build/default/_doc/_odoc/my_pkg/my_pkg.__private__.priv/priv.odoc
  ./_build/default/_doc/_odoc/my_pkg/my_pkg.bar/bar.odoc
  ./_build/default/_doc/_odoc/my_pkg/my_pkg.foo/foo.odoc
  ./_build/default/_doc/_odoc/my_pkg/my_pkg.foo/foo__A.odoc
  ./_build/default/_doc/_odoc/my_pkg/my_pkg.foo/foo__B.odoc
  ./_build/default/_doc/_odoc/my_pkg/page-index.odoc
  ./_build/default/_doc/_odoc/solo@e3b9de2fb703/solo.odoc

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
