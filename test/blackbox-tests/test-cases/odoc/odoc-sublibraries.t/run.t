Test that sublibraries (like mylib.sub) get correct odoc paths.
The parent library is mylib, and the sublibrary is mylib.sub.

Build documentation:

  $ dune build @doc

Verify both libraries are documented:

  $ find _build/default/_doc/_odocl/mylib -name '*.odocl' | sort -n
  _build/default/_doc/_odocl/mylib/mylib.sub/mylib_sub.odocl
  _build/default/_doc/_odocl/mylib/mylib.sub/page-index.odocl
  _build/default/_doc/_odocl/mylib/mylib/mylib.odocl
  _build/default/_doc/_odocl/mylib/mylib/page-index.odocl
  _build/default/_doc/_odocl/mylib/page-index.odocl

Check HTML structure for parent library:

  $ ls _build/default/_doc/_html/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html/mylib/mylib/Mylib/index.html

Check HTML structure for sublibrary:

  $ ls _build/default/_doc/_html/mylib/mylib.sub/Mylib_sub/index.html
  _build/default/_doc/_html/mylib/mylib.sub/Mylib_sub/index.html

Verify the package structure is correct (both under mylib package):

  $ ls -d _build/default/_doc/_odoc/mylib/mylib
  _build/default/_doc/_odoc/mylib/mylib

  $ ls -d _build/default/_doc/_odoc/mylib/mylib.sub
  _build/default/_doc/_odoc/mylib/mylib.sub
