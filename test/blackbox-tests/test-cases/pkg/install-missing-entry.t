Test missing entries in the .install file

  $ . ./helpers.sh

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
  $ build_pkg test 2>&1 | sed 's#_build.*_private#$ROOT/_private#'
  Error: entry
  $ROOT/_private/default/.pkg/test.0.0.1-962f24dc2cb394442fe86368a850a9d0/source/myfile
  in
  $ROOT/_private/default/.pkg/test.0.0.1-962f24dc2cb394442fe86368a850a9d0/source/test.install
  does not exist
  -> required by
     $ROOT/_private/default/.pkg/test.0.0.1-962f24dc2cb394442fe86368a850a9d0/target

This on the other hand shouldn't error because myfile is optional

  $ lockfile "?myfile"
  $ build_pkg test
