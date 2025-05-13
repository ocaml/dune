A usefull pattern for documentation is to have a folder that contains only the
documentation, and to include everything in it with:

  $ cat dune
  (documentation
   (files
    (glob_files_rec
     (doc/* with_prefix .))))

The whole content of the doc folder is included as mld files/assets, replacing the doc/ prefix with "./"

Let's verify that:

  $ dune build @install

We store the hierarchy of the doc/odoc-pages/ installed folder
  $ cd _build/install/default/doc/testing_mld/odoc-pages
  $ ls -R > ../../../../../../installed_hierarchy
  $ cd ../../../../../../
We store the original hierarchy of the doc/ folder
  $ cd doc
  $ ls -R > ../source_hierarchy
  $ cd ../
We compare both, they should be equal
  $ diff source_hierarchy installed_hierarchy

For the curious reader, here is the hierarchy:
  $ cat source_hierarchy
  .:
  examples
  index.mld
  tutorial
  
  ./examples:
  example1
  example2
  index.mld
  summary.mld
  
  ./examples/example1:
  index.mld
  
  ./examples/example2:
  index.mld
  
  ./tutorial:
  tuto1.mld

Let's now verify that the install file is correct:
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
    "_build/install/default/doc/testing_mld/odoc-pages/index.mld" {"odoc-pages/index.mld"}
    "_build/install/default/doc/testing_mld/odoc-pages/tutorial/tuto1.mld" {"odoc-pages/tutorial/tuto1.mld"}
  ]

Even though dune does not support yet building the doc with hierarchy, I can't
resist building the doc to check what happens: currently, only top-level mld
files are included in the doc generation.

  $ dune build @doc
  $ ls _build/default/_doc/_html/testing_mld
  db.js
  index.html
