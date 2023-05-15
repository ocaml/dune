Test missing entries in the .install file

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ lockfile() {
  > cat >dune.lock/test <<EOF
  > (build
  >  (system "echo 'lib: [ \"$1\" ]' > test.install"))
  > EOF
  > }

This should give us a proper error that myfile wasn't generated

  $ lockfile "myfile"
  $ dune build .pkg/test/target/
  Error: No such file or directory
  -> required by _build/default/.pkg/test/target
  [1]

This on the other hand shouldn't error because myfile is optional

  $ lockfile "?myfile"
  $ dune build .pkg/test/target/
