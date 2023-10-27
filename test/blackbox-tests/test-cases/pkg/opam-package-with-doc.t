We test how a package using the `with-doc` variable behaves.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg  

  $ mkpkg with-doc <<EOF
  > depends: [ "doc-dep" {with-doc} ]
  > build: [ "echo" "Building with-doc" {with-doc} ]
  > EOF
  $ mkpkg doc-dep <<EOF
  > build: [ "echo" "Building doc-dep" ]
  > EOF

The doc dependency is currently ignored. We also make sure to translate the filter.
  $ solve_translate_opam_filters with-doc
  Solution for dune.lock:
  - with-doc.0.0.1

The filter is translated correctly.
  $ cat dune.lock/with-doc.pkg 
  (version 0.0.1)
  
  (build
   (run
    echo
    (when %{pkg-self:with-doc} "Building with-doc")))

Package doesn't build due to missing value of `with-doc`.
  $ dune build
  File "dune.lock/with-doc.pkg", line 6, characters 8-28:
  6 |   (when %{pkg-self:with-doc} "Building with-doc")))
              ^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-doc"
  [1]
