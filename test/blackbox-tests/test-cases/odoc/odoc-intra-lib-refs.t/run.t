Test that intra-library module references work correctly in a wrapped library.
Module Foo references module Bar, both are submodules of the wraplib library.

Build documentation:

  $ dune build @doc

Verify documentation was generated (wrapped modules compile but don't link separately):

  $ find _build/default/_doc/_odoc/wraplib -name '*.odoc' | sort -n
  _build/default/_doc/_odoc/wraplib/page-index.odoc
  _build/default/_doc/_odoc/wraplib/wraplib/page-index.odoc
  _build/default/_doc/_odoc/wraplib/wraplib/wraplib.odoc
  _build/default/_doc/_odoc/wraplib/wraplib/wraplib__Bar.odoc
  _build/default/_doc/_odoc/wraplib/wraplib/wraplib__Foo.odoc

Only the wrapper module gets linked:

  $ find _build/default/_doc/_odocl/wraplib -name '*.odocl' | sort -n
  _build/default/_doc/_odocl/wraplib/page-index.odocl
  _build/default/_doc/_odocl/wraplib/wraplib/page-index.odocl
  _build/default/_doc/_odocl/wraplib/wraplib/wraplib.odocl

Check that HTML was generated for the main module:

  $ ls _build/default/_doc/_html/wraplib/wraplib/Wraplib/index.html
  _build/default/_doc/_html/wraplib/wraplib/Wraplib/index.html

Verify that the main module exposes the submodules:

  $ ls _build/default/_doc/_html/wraplib/wraplib/Wraplib/Foo/index.html
  _build/default/_doc/_html/wraplib/wraplib/Wraplib/Foo/index.html

  $ ls _build/default/_doc/_html/wraplib/wraplib/Wraplib/Bar/index.html
  _build/default/_doc/_html/wraplib/wraplib/Wraplib/Bar/index.html

Verify that all intra-library references resolved correctly (no unresolved xrefs):

  $ grep -r "xref-unresolved" _build/default/_doc/_html/wraplib/wraplib/Wraplib/
  [1]
