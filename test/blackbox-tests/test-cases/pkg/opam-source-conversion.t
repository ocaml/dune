Test conversion of opam sources into lock dir package specifications

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
  >   local f="${default_lock_dir}"/testpkg.0.0.1.pkg
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

  $ solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1

  $ showpkg | dune_cmd subst "$PWD" '<pwd>'
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

  $ solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url hg+http://no-support.com/foo)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

Create a local git repo to resolve to

  $ mkdir _repo
  $ git -C _repo init --initial-branch=main --quiet
  $ touch _repo/content
  $ git -C _repo add -A
  $ git -C _repo commit -m "Initial commit" --quiet
  $ expected_hash=$(git -C _repo rev-parse HEAD)

git+http

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+http://github.com/foo"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF

  $ mkdir _bin
  $ cp fakegit.sh _bin/git

  $ PATH=_bin:$PATH solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url git+http://github.com/foo#058003b274f05b092a391555ffdee8b36f1897b3)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

git+file

  $ rm -rf ${default_lock_dir}

  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+file://$PWD/_repo"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1
  $ showpkg | dune_cmd subst "$PWD" '$PWD' | dune_cmd subst "$expected_hash" '$EXPECTED_HASH'
  (version 0.0.1)
  
  (source
   (fetch
    (url
     git+file://$PWD/_repo#$EXPECTED_HASH)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

git+foobar

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+foobar://random-thing-here"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ PATH=_bin:$PATH solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
  
  (source
   (fetch
    (url
     git+foobar://random-thing-here#058003b274f05b092a391555ffdee8b36f1897b3)
    (checksum md5=069aa55d40e548280f92af693f6c625a)))

file+git

  $ rm -rf ${default_lock_dir}
  $ mkpkg testpkg <<EOF
  > url {
  >   src: "file+git://random-thing-here"
  >   checksum: "md5=069aa55d40e548280f92af693f6c625a"
  > }
  > EOF
  $ solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1
  $ showpkg
  (version 0.0.1)
