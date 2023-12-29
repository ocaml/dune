Test conversion of opam sources into lock dir package specifications

  $ . ./helpers.sh

  $ mkrepo

  $ mkpkg testpkg <<EOF
  > url {
  >   src: "http://caml.inria.fr/pub/distrib/ocaml-3.11/ocaml-3.11.1.tar.gz"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF

  $ solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1

  $ showpkg() {
  > local f=dune.lock/testpkg.pkg
  > [ -e $f ] && cat $f
  > }
 
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url http://caml.inria.fr/pub/distrib/ocaml-3.11/ocaml-3.11.1.tar.gz)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

  $ mkdir testpkgsources

  $ mkpkg testpkg <<EOF
  > url {
  >   src: "file://$PWD/testpkgsources"
  > }
  > EOF

  $ rm -rf dune.lock

  $ solve testpkg 2>&1 | sed -E 's#.*.sandbox/[^/]+#.sandbox/$SANDBOX#g' | sed '/File "/q'
  Package "testpkg" has source archive which lacks a checksum.
  The source archive will be downloaded from:
  .sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/testpkgsources
  Dune will compute its own checksum for this source archive.
  File "vendor/opam/src/repository/opamRepository.ml", line 405, characters 31-37:

  $ showpkg
  [1]
