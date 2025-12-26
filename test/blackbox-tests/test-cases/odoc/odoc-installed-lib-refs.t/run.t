Test that references to installed libraries like Lwt work correctly.
This verifies that cross-library references in documentation comments
are resolved correctly when linking.

Build documentation:

  $ dune build @doc

Check that documentation was generated without broken reference warnings:

  $ find _build/default/_doc/_odocl/mylib -name '*.odocl' | sort -n
  _build/default/_doc/_odocl/mylib/mylib/mylib.odocl
  _build/default/_doc/_odocl/mylib/mylib/page-index.odocl
  _build/default/_doc/_odocl/mylib/page-index.odocl

Check that HTML was generated for our library:

  $ find _build/default/_doc/_html/mylib -name '*.html' | sort -n
  _build/default/_doc/_html/mylib/index.html
  _build/default/_doc/_html/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html/mylib/mylib/index.html

Verify that Lwt documentation was also built (needed for cross-references):

  $ ls _build/default/_doc/_odoc/lwt/lwt/Lwt.odoc
  _build/default/_doc/_odoc/lwt/lwt/Lwt.odoc

Check that the generated HTML contains links to Lwt types:

  $ grep -o "href=\"[^\"]*Lwt[^\"]*\"" _build/default/_doc/_html/mylib/mylib/Mylib/index.html | head -3
  href="https://ocaml.org/p/lwt/5.9.2/doc/lwt/Lwt/index.html#type-t"
  href="https://ocaml.org/p/lwt/5.9.2/doc/lwt/Lwt/index.html#type-t"
  href="https://ocaml.org/p/lwt/5.9.2/doc/lwt/Lwt/index.html#type-t"
