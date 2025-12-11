Test that cross-package references work between local packages.
Package bar depends on package foo, and bar's documentation references foo's types.

Build documentation for both packages:

  $ dune build @doc

Verify documentation was generated for both packages:

  $ find _build/default/_doc/_odocl/{foo,bar} -name '*.odocl' | sort -n
  _build/default/_doc/_odocl/bar/bar.lib/barlib.odocl
  _build/default/_doc/_odocl/bar/bar.lib/page-index.odocl
  _build/default/_doc/_odocl/bar/page-index.odocl
  _build/default/_doc/_odocl/foo/foo.lib/foolib.odocl
  _build/default/_doc/_odocl/foo/foo.lib/page-index.odocl
  _build/default/_doc/_odocl/foo/page-index.odocl

Check that HTML was generated for both libraries:

  $ ls _build/default/_doc/_html/foo/foo.lib/Foolib/index.html
  _build/default/_doc/_html/foo/foo.lib/Foolib/index.html

  $ ls _build/default/_doc/_html/bar/bar.lib/Barlib/index.html
  _build/default/_doc/_html/bar/bar.lib/Barlib/index.html

Verify that bar's HTML contains links to foo's documentation:

  $ grep -o "href=\"[^\"]*Foolib[^\"]*\"" _build/default/_doc/_html/bar/bar.lib/Barlib/index.html | head -3
  href="../../../foo/foo.lib/Foolib/index.html#type-foo_type"
  href="../../../foo/foo.lib/Foolib/index.html#type-foo_type"
  href="../../../foo/foo.lib/Foolib/index.html#val-foo_function"

Check that the -P flags are passed correctly during linking.
Only bar's own package is passed as -P (foo is not, since there's no explicit doc dependency):

  $ dune clean
  $ dune build @doc --verbose 2>&1 | grep "odoc link.*barlib.odocl" | grep -o "\-P [^ ]*" | sort | uniq
  -P bar:_odoc/bar
