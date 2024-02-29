Test that dune supports lockfiles with md5, sha256 and sha512 hashes.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg with-md5 <<EOF
  > url {
  >  src: "file://with-md5"
  >  checksum: [
  >   "md5=764efa883dda1e11db47671c4a3bbd9e"
  >  ]
  > }
  > EOF

  $ mkpkg with-sha256 <<EOF
  > url {
  >  src: "file://with-sha256"
  >  checksum: [
  >   "sha256=98ea6e4f216f2fb4b69fff9b3a44842c38686ca685f3f55dc48c5d3fb1107be4"
  >  ]
  > }
  > EOF

  $ mkpkg with-sha512 <<EOF
  > url {
  >  src: "file://with-sha512"
  >  checksum: [
  >   "sha512=d78abb0542736865f94704521609c230dac03a2f369d043ac212d6933b91410e06399e37f9c5cc88436a31737330c1c8eccb2c2f9f374d62f716432a32d50fac"
  >  ]
  > }
  > EOF

This package uses multiple hashing algorithms. Currently dune will just add the
first checksum to the lockfile for this package.
  $ mkpkg with-all <<EOF
  > url {
  >  src: "file://with-all"
  >  checksum: [
  >   "md5=764efa883dda1e11db47671c4a3bbd9e"
  >   "sha256=98ea6e4f216f2fb4b69fff9b3a44842c38686ca685f3f55dc48c5d3fb1107be4"
  >   "sha512=d78abb0542736865f94704521609c230dac03a2f369d043ac212d6933b91410e06399e37f9c5cc88436a31737330c1c8eccb2c2f9f374d62f716432a32d50fac"
  >  ]
  > }
  > EOF

  $ solve with-md5 with-sha256 with-sha512 with-all
  Solution for dune.lock:
  - with-all.0.0.1
  - with-md5.0.0.1
  - with-sha256.0.0.1
  - with-sha512.0.0.1

  $ cat dune.lock/*
  (lang package 0.1)
  
  (dependency_hash 4435ebc0724374e2f76349d7a2e7ab6e)
  
  (repositories
   (complete false)
   (used))
  (version 0.0.1)
  
  (source
   (fetch
    (url file://with-all)
    (checksum md5=764efa883dda1e11db47671c4a3bbd9e)))
  (version 0.0.1)
  
  (source
   (fetch
    (url file://with-md5)
    (checksum md5=764efa883dda1e11db47671c4a3bbd9e)))
  (version 0.0.1)
  
  (source
   (fetch
    (url file://with-sha256)
    (checksum
     sha256=98ea6e4f216f2fb4b69fff9b3a44842c38686ca685f3f55dc48c5d3fb1107be4)))
  (version 0.0.1)
  
  (source
   (fetch
    (url file://with-sha512)
    (checksum
     sha512=d78abb0542736865f94704521609c230dac03a2f369d043ac212d6933b91410e06399e37f9c5cc88436a31737330c1c8eccb2c2f9f374d62f716432a32d50fac)))
