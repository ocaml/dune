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
  $ build_pkg test 2>&1 | dune_cmd subst '_build.*_private' '$ROOT/_private' | censor
  Error: entry
  $ROOT/_private/default/.pkg/test.0.0.1-$DIGEST/source/myfile
  in
  $ROOT/_private/default/.pkg/test.0.0.1-$DIGEST/source/test.install
  does not exist
  -> required by
     $ROOT/_private/default/.pkg/test.0.0.1-$DIGEST/target
  [1]

This on the other hand shouldn't error because myfile is optional

  $ lockfile "?myfile"
  $ build_pkg test
