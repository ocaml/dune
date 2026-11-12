Test missing entries in the .install file

  $ make_lockdir
  $ lockfile() {
  > make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "echo 'lib: [ \"$1\" ]' > test.install"))
  > EOF
  > }

This should give us a proper error that myfile wasn't generated

  $ lockfile "myfile"
  $ build_pkg test 2>&1 | censor
  Error: entry
  _build/.sandbox/$DIGEST/default/.lockfile/pkg/test/.opam/test/source/myfile
  in
  _build/.sandbox/$DIGEST/default/.lockfile/pkg/test/.opam/test/source/test.install
  does not exist
  -> required by _build/default/.lockfile/pkg/test/.opam/test/target
  [1]

This on the other hand shouldn't error because myfile is optional

  $ lockfile "?myfile"
  $ build_pkg test
