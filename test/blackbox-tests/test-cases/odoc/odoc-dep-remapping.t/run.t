Test that references to installed library types are remapped correctly.
This verifies that cross-library references in documentation comments
are resolved to ocaml.org URLs when building with @doc.

Build documentation:

  $ dune build @doc

Check that documentation was generated:

  $ ls _build/default/_doc/_html/testpkg/testpkg.lib/Testlib/index.html
  _build/default/_doc/_html/testpkg/testpkg.lib/Testlib/index.html

Check that references to Odoc_parser types are remapped to ocaml.org:

  $ grep -o 'href="[^"]*Odoc_parser[^"]*"' _build/default/_doc/_html/testpkg/testpkg.lib/Testlib/index.html | sed 's|/[0-9.]*-*[a-z0-9]*/doc|/VERSION/doc|g' | sort -u
  href="https://ocaml.org/p/odoc-parser/VERSION/doc/odoc-parser/Odoc_parser/Loc/index.html#type-span"
  href="https://ocaml.org/p/odoc-parser/VERSION/doc/odoc-parser/Odoc_parser/Warning/index.html#type-t"
  href="https://ocaml.org/p/odoc-parser/VERSION/doc/odoc-parser/Odoc_parser/index.html#type-t"
  href="https://ocaml.org/p/odoc-parser/VERSION/doc/odoc-parser/Odoc_parser/index.html#val-parse_comment"

Check that page references to documentation dependencies are also remapped:

  $ grep -o 'href="[^"]*odoc_for_authors[^"]*"' _build/default/_doc/_html/testpkg/testpkg.lib/Testlib/index.html | sed 's|/[0-9.]*-*[a-z0-9]*/doc|/VERSION/doc|g'
  href="../../../odoc/odoc_for_authors.html"
