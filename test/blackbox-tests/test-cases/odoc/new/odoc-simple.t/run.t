This test generates documentation using odoc for a library:

  $ dune build @doc-new

This test if `.odocl` files are generated
  $ find _build/default/_doc_new/odoc/local -name '*.odocl' | sort -n
  _build/default/_doc_new/odoc/local/bar/bar.odocl
  _build/default/_doc_new/odoc/local/foo/byte/foo_byte.odocl
  _build/default/_doc_new/odoc/local/foo/foo.odocl
  _build/default/_doc_new/odoc/local/foo/foo2.odocl

  $ ls _build/default/_doc_new/html/docs/local/
  bar
  foo
  index.html

  $ dune build @foo-mld
  {0 Package foo}
  {1 Sub-indexes}
  - {{!page-"byte"}byte}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:foo foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ dune build @bar-mld
  {0 Package bar}
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.
