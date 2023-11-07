This test demonstrates a package where the .install file being created by the
file copying step rather than the build step.

  $ . ./helpers.sh

  $ mkdir -p dune.lock/foo.files
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF

  $ touch dune.lock/foo.files/foo.install dune.lock/foo.pkg

The foo.install file in files/ should have been copied over.
  $ build_pkg foo 2>&1 | sed 's/copyfile/open/'
  Error:
  open(_build/_private/default/.pkg/foo/source/foo.install): No such file or directory
  -> required by _build/_private/default/.pkg/foo/target/cookie
