Test that package mld files can reference modules from other packages.
Package A's index.mld contains a reference to {!Libb} from package B.
With documentation dependencies declared in dune-project, cross-package references resolve correctly.

Build documentation:

  $ dune build @doc

Verify documentation was generated for both packages:

  $ find _build/default/_doc/_odocl/{pkga,pkgb} -name '*.odocl' | sort -n
  _build/default/_doc/_odocl/pkga/page-index.odocl
  _build/default/_doc/_odocl/pkga/pkga.lib/liba.odocl
  _build/default/_doc/_odocl/pkga/pkga.lib/page-index.odocl
  _build/default/_doc/_odocl/pkgb/page-index.odocl
  _build/default/_doc/_odocl/pkgb/pkgb.lib/libb.odocl
  _build/default/_doc/_odocl/pkgb/pkgb.lib/page-index.odocl

Check that HTML was generated for both package indexes:

  $ ls _build/default/_doc/_html/pkga/index.html
  _build/default/_doc/_html/pkga/index.html

  $ ls _build/default/_doc/_html/pkgb/index.html
  _build/default/_doc/_html/pkgb/index.html

Both libraries' HTML should be generated with cross-package references resolved:

  $ ls _build/default/_doc/_html/pkga/pkga.lib/Liba/index.html
  _build/default/_doc/_html/pkga/pkga.lib/Liba/index.html

  $ ls _build/default/_doc/_html/pkgb/pkgb.lib/Libb/index.html
  _build/default/_doc/_html/pkgb/pkgb.lib/Libb/index.html

Verify that all cross-package references resolved correctly (no unresolved xrefs):

  $ grep -r "xref-unresolved" _build/default/_doc/_html/pkga/ _build/default/_doc/_html/pkgb/
  [1]
