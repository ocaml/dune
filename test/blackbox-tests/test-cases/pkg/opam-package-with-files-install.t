This test demonstrates a package where the .install file being created by the
file copying step rather than the build step.

  $ . ./helpers.sh

  $ mkdir -p dune.lock/foo.files
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF

  $ touch dune.lock/foo.files/foo.install dune.lock/foo.pkg

The foo.install file in files/ has been copied over allowing the build to succeed.
  $ build_pkg foo
