Test hierarchical mld files (mlds in subdirectories)

  $ dune build @doc
  File "../doc/mypkg.mld", line 7, characters 9-50:
  Warning: Failed to resolve reference unresolvedroot(getting-started) Couldn't find page "getting-started"

Check what HTML was generated - hierarchical pages ARE supported:

  $ ls _build/default/_doc/_html/mypkg/
  index.html
  mypkg.html
  tutorial

  $ ls _build/default/_doc/_html/mypkg/tutorial/
  getting-started.html

Verify the content was rendered:

  $ cat _build/default/_doc/_html/mypkg/tutorial/getting-started.html | grep -o 'Getting Started'
  Getting Started
  Getting Started
  Getting Started

Verify the cross-references from mypkg.mld to the tutorial page resolved correctly:

  $ grep -o 'href="[^"]*getting-started[^"]*"' _build/default/_doc/_html/mypkg/mypkg.html
  href="tutorial/getting-started.html"
  href="tutorial/getting-started.html"
  href="tutorial/getting-started.html"
  href="tutorial/getting-started.html"

The intentionally broken reference (without tutorial/ path) shows as unresolved:

  $ grep -o 'xref-unresolved[^>]*>[^<]*' _build/default/_doc/_html/mypkg/mypkg.html
  xref-unresolved" title="getting-started">shouldn't work
