We make sure that file like foo.install are correctly included from the files
subdirectory of an opam repository. Notably, the build step here is empty. The
library seq is such an example in the wild with this behaviour.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg foo
  $ mkdir -p $mock_packages/foo/foo.0.0.1/files
  $ cat > $mock_packages/foo/foo.0.0.1/files/foo.install

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

The lock directory contains the .install file as expected.
  $ [ -f dune.lock/foo.files/foo.install ]

The pkg file is empty, there is no build stanza.
  $ cat dune.lock/foo.pkg
  (version 0.0.1)

The foo.install file in files/ should have been copied over.
  $ build_pkg foo 2> /dev/null
  [1]
