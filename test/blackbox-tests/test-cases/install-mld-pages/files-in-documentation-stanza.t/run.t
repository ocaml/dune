The files field of the documentation stanza allows to control the hierarchy
of documentation files

See doc/dune: here, the mld pages in tutorial/ will be installed in
<page_root>/tutorial/tuto1/ while the ones in doc/ will be installed in
<page_root>/ (where <page_root> is <opam switch root>/doc/<pkgname>/)

Let's verify that:

  $ dune build @install
  $ ls -F _build/install/default/doc/testing_mld/odoc-pages
  examples/
  img1.png@
  img2.png@
  index.mld@
  notes.mld@
  tutorial/
  $ ls _build/install/default/doc/testing_mld/odoc-pages/tutorial/
  tuto1.mld
  tuto2.mld

  $ cat _build/default/testing_mld.install
  lib: [
    "_build/install/default/lib/testing_mld/META"
    "_build/install/default/lib/testing_mld/dune-package"
  ]
  doc: [
    "_build/install/default/doc/testing_mld/odoc-pages/examples/example1/index.mld" {"odoc-pages/examples/example1/index.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/examples/example2/index.mld" {"odoc-pages/examples/example2/index.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/examples/index.mld" {"odoc-pages/examples/index.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/examples/summary.mld" {"odoc-pages/examples/summary.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/img1.png" {"odoc-pages/img1.png"}
    "_build/install/default/doc/testing_mld/odoc-pages/img2.png" {"odoc-pages/img2.png"}
    "_build/install/default/doc/testing_mld/odoc-pages/index.mld" {"odoc-pages/index.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/notes.mld" {"odoc-pages/notes.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/tutorial/tuto1.mld" {"odoc-pages/tutorial/tuto1.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/tutorial/tuto2.mld" {"odoc-pages/tutorial/tuto2.mld"}
  ]

Even though dune does not support yet building the doc with hierarchy, I can't
resist building the doc to check what happens: currently, only top-level mld
files are included in the doc generation.

  $ dune build @doc
  Warning: Dune does not yet support building documentation for assets, and
  mlds in a non-flat hierarchy. Ignoring examples/example1/index.mld,
  examples/example2/index.mld, examples/index.mld, examples/summary.mld,
  img1.png, img2.png, tutorial/tuto1.mld, tutorial/tuto2.mld.
  $ ls _build/default/_doc/_html/testing_mld
  db.js
  index.html
  notes.html
