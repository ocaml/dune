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
 
  $ cat dune.lock/testpkg.pkg
  (version 0.0.1)
  
  (source
   (fetch
    (url http://caml.inria.fr/pub/distrib/ocaml-3.11/ocaml-3.11.1.tar.gz)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))
