This test generates documentation using odoc for a library:

  $ dune build @doc

This test if `.odocl` files are generated
  $ find _build/default/_doc/_odocl/{bar,foo} -name '*.odocl' | sort -n
  _build/default/_doc/_odocl/bar/bar/bar.odocl
  _build/default/_doc/_odocl/bar/bar/page-index.odocl
  _build/default/_doc/_odocl/bar/page-index.odocl
  _build/default/_doc/_odocl/foo/foo.byte/foo_byte.odocl
  _build/default/_doc/_odocl/foo/foo.byte/page-index.odocl
  _build/default/_doc/_odocl/foo/foo/foo.odocl
  _build/default/_doc/_odocl/foo/foo/foo2.odocl
  _build/default/_doc/_odocl/foo/foo/foo3.odocl
  _build/default/_doc/_odocl/foo/foo/page-index.odocl
  _build/default/_doc/_odocl/foo/page-index.odocl

Verify HTML index was generated:

  $ test -f _build/default/_doc/_html/index.html && echo "index.html exists"
  index.html exists

  $ dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!/foo.byte/module-Foo_byte}.

  $ dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!/bar/module-Bar}.
