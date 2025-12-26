Test that a specific odoc v3 file can be built directly

  $ dune build _build/default/_doc/_odoc/foo/foo/foo.odoc
  $ ls _build/default/_doc/_odoc/foo/foo/
  Foo.deps
  foo.odoc

Verify the odoc file was compiled with v3 flags (--parent-id and --output-dir)

  $ dune clean
  $ dune build _build/default/_doc/_odoc/foo/foo/foo.odoc --verbose 2>&1 | grep -o "\-\-output-dir [^ ]* \-\-parent-id foo/foo"
  --output-dir _odoc --parent-id foo/foo

Test that odocl files are generated in v3 structure

  $ dune build _build/default/_doc/_odocl/foo/foo/foo.odocl
  $ ls _build/default/_doc/_odocl/foo/foo/
  foo.odocl

Test that HTML files are generated in v3 structure

  $ dune build _build/default/_doc/_html/foo/foo/Foo/index.html
  $ ls _build/default/_doc/_html/foo/foo/Foo/
  index.html
