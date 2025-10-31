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
  Solution for .dune-solution-cache:
  - testpkg.0.0.1

  $ showpkg() {
  >   local f="${default_lock_dir}"/testpkg.pkg
  >   [ -e $f ] && cat $f
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

  $ rm -rf ${default_lock_dir}

  $ solve testpkg 2>&1 | sed -E 's#.*.sandbox/[^/]+#.sandbox/$SANDBOX#g' | sed '/File "/q'
  Solution for .dune-solution-cache:
  - testpkg.0.0.1

  $ showpkg | sed -e "s#$PWD#<pwd>#"
  (version 0.0.1)
  
  (source
   (fetch
    (url
     file://<pwd>/testpkgsources)))
  
  (dev)

Unsupported backends:

  $ rm -rf ${default_lock_dir}

  $ mkpkg testpkg <<EOF
  > url {
  >   src: "hg+http://no-support.com/foo"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF

  $ solve testpkg 2>&1
  Solution for .dune-solution-cache:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url hg+http://no-support.com/foo)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

git+http

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+http://github.com/foo"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF

  $ solve testpkg 2>&1
  Solution for .dune-solution-cache:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url git+http://github.com/foo)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

git+file

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+file://here"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ solve testpkg 2>&1
  Solution for .dune-solution-cache:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url git+file://here)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

git+foobar

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+foobar://random-thing-here"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ solve testpkg 2>&1
  Solution for .dune-solution-cache:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url git+foobar://random-thing-here)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

file+git

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "file+git://random-thing-here"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ solve testpkg 2>&1
  Solution for .dune-solution-cache:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
