Test missing entries in the .install file

  $ . ./helpers.sh

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ lockfile() {
  > cat >dune.lock/test.pkg <<EOF
  > (build
  >  (system "echo 'lib: [ \"$1\" ]' > test.install"))
  > EOF
  > }

This should give us a proper error that myfile wasn't generated

  $ lockfile "myfile"
  $ build_pkg test
  Error: No such file or directory
  -> required by _build/_private/default/.pkg/test/target
  [1]

This on the other hand shouldn't error because myfile is optional

  $ lockfile "?myfile"
  $ build_pkg test
