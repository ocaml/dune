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
  $ build_pkg test 2>&1 | dune_cmd subst '_build.*_private' '$ROOT/_private'
  Error: entry
  $ROOT/_private/default/.pkg/test.0.0.1-14cf8b955e694dcf79c50e1c47a4d853/source/myfile
  in
  $ROOT/_private/default/.pkg/test.0.0.1-14cf8b955e694dcf79c50e1c47a4d853/source/test.install
  does not exist
  -> required by
     $ROOT/_private/default/.pkg/test.0.0.1-14cf8b955e694dcf79c50e1c47a4d853/target
  [1]

This on the other hand shouldn't error because myfile is optional

  $ lockfile "?myfile"
  $ build_pkg test
