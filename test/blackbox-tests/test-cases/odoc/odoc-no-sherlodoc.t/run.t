This test generates documentation using odoc for a library, and hides any
sherlodoc installation to test what happens when sherlodoc is not installed.

Set up some fake environment without sherlodoc

  $ mkdir _path
  $ ln -s $(command -v dune) _path/
  $ ln -s $(command -v ocamlc) _path/
  $ ln -s $(command -v ocamldep) _path/
  $ ln -s $(command -v odoc) _path/
  $ PATH=$(realpath ./_path) dune build @doc

This test if `.odocl` files are generated
  $ find _build/default/_doc/_odocl -name '*.odocl' | grep -E '/(bar|foo)/' | sort -n
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

  $ find . -name '*.js' | sort -n
  ./_build/default/_doc/_html/odoc.support/highlight.pack.js
  ./_build/default/_doc/_html/odoc.support/katex.min.js
  ./_build/default/_doc/_html/odoc.support/odoc_search.js


Verify HTML index was generated:

  $ test -f _build/default/_doc/_html/index.html && echo "index.html exists"
  index.html exists

  $ PATH=$(realpath ./_path) dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!/foo.byte/module-Foo_byte}.

  $ PATH=$(realpath ./_path) dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!/bar/module-Bar}.

