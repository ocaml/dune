Test the behavior of multiple checksums when locking

  $ . ./helpers.sh

To begin with we create an opam package that has multiple checksums using
different hash algorithms as well as multiple

  $ mkrepo
  $ mkpkg multiple-checksums <<EOF
  > url {
  >    src: "https://unimportant.url/never-used.tar.gz"
  >    checksum: [
  >      "md5=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  >      "sha256=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  >      "sha512=cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
  >    ]
  > }
  > extra-source "fixes.patch" {
  >    src: "https://unimportant.url/unused.patch"
  >    checksum: [
  >      "md5=dddddddddddddddddddddddddddddddd"
  >      "sha256=eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  >      "sha512=ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  >    ]
  > }
  > EOF

Then we create out own project which depends on that OPAM package.

  $ cat > dune-project <<EOF
  > (lang dune 3.15)
  > (package (name my) (depends multiple-checksums))
  > EOF

Locking should work

  $ add_mock_repo_if_needed
  $ dune pkg lock
  Solution for dune.lock:
  - multiple-checksums.0.0.1

The generated lock file should contain all the sources with at least one
checksum:

  $ cat dune.lock/multiple-checksums.pkg
  (version 0.0.1)
  
  (source
   (fetch
    (url https://unimportant.url/never-used.tar.gz)
    (checksum md5=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)))
  
  (extra_sources
   (fixes.patch
    (fetch
     (url https://unimportant.url/unused.patch)
     (checksum md5=dddddddddddddddddddddddddddddddd))))
