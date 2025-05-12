The files field of the documentation stanza allows to control the hierarchy
of documentation files

  $ cat doc/dune
  (documentation
   (mld_files) ; we avoid adding files twice, once through mld_files and once through files
   (files
    (glob_files
     (assets/* with_prefix .))
    ; Assets in the source tree are in a subdirectory, but installed at the root of the doc
    (glob_files_rec examples/*) ; The examples in the source tree matches the one installed
    (glob_files
     (tuto*.mld with_prefix tutorial))
    ; Tutorial files are in a subdir in the doc hierarchy
    (pkgname.mld as index.mld) ; pkgname.mld is renamed in order to be the index file
    ))

Here, the mld pages in tutorial/ will be installed in
<page_root>/tutorial/tuto1/ while the ones in doc/ will be installed in
<page_root>/ (where <page_root> is <opam switch root>/doc/<pkgname>/)

Let's verify that:

  $ dune build @install
  $ ls -F _build/install/default/doc/testing_mld/odoc-pages
  examples/
  img1.png@
  img2.png@
  index.mld@
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
    "_build/install/default/doc/testing_mld/odoc-pages/tutorial/tuto1.mld" {"odoc-pages/tutorial/tuto1.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/tutorial/tuto2.mld" {"odoc-pages/tutorial/tuto2.mld"}
  ]
