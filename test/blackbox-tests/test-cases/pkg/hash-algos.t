Test that dune supports lockfiles with md5, sha256 and sha512 hashes.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg with-md5 <<EOF
  > url {
  >  src: "file://with-md5"
  >  checksum: [
  >   "md5=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  >  ]
  > }
  > EOF

  $ mkpkg with-sha256 <<EOF
  > url {
  >  src: "file://with-sha256"
  >  checksum: [
  >   "sha256=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  >  ]
  > }
  > EOF

  $ mkpkg with-sha512 <<EOF
  > url {
  >  src: "file://with-sha512"
  >  checksum: [
  >   "sha512=cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
  >  ]
  > }
  > EOF

This package uses multiple hashing algorithms. Currently dune will just add the
first checksum to the lockfile for this package.

  $ mkpkg with-all <<EOF
  > url {
  >  src: "file://with-all"
  >  checksum: [
  >   "md5=dddddddddddddddddddddddddddddddd"
  >   "sha256=eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  >   "sha512=ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  >  ]
  > }
  > extra-source "fixes.patch" {
  >    src: "https://unimportant.url/unused.patch"
  >    checksum: [
  >      "md5=00000000000000000000000000000000"
  >      "sha256=1111111111111111111111111111111111111111111111111111111111111111"
  >      "sha512=22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222"
  >    ]
  > }
  > EOF

  $ solve with-md5 with-sha256 with-sha512 with-all
  Solution for .dune-solution-cache:
  - with-all.0.0.1
  - with-md5.0.0.1
  - with-sha256.0.0.1
  - with-sha512.0.0.1

  $ cat ${default_lock_dir}/*
  (lang package 0.1)
  
  (dependency_hash 32180cf311133b30d0b5be2a40c89f43)
  
  (repositories
   (complete false)
   (used))
  (version 0.0.1)
  
  (source
   (fetch
    (url file://with-all)
    (checksum md5=dddddddddddddddddddddddddddddddd)))
  
  (extra_sources
   (fixes.patch
    (fetch
     (url https://unimportant.url/unused.patch)
     (checksum md5=00000000000000000000000000000000))))
  (version 0.0.1)
  
  (source
   (fetch
    (url file://with-md5)
    (checksum md5=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)))
  (version 0.0.1)
  
  (source
   (fetch
    (url file://with-sha256)
    (checksum
     sha256=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)))
  (version 0.0.1)
  
  (source
   (fetch
    (url file://with-sha512)
    (checksum
     sha512=cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc)))
