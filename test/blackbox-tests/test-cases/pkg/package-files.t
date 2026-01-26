Additional files overlaid on top of the source can be found in the
%pkg.files/ directory:

  $ mkdir test-source
  $ touch test-source/foo

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source
  >  (copy $PWD/test-source))
  > (build
  >  (system "echo foo:; cat foo; echo bar:; cat bar"))
  > EOF

  $ make_lockpkg_file test foo <<EOF
  > foo from test.files
  > EOF
  $ make_lockpkg_file test bar <<EOF
  > bar from test.files
  > EOF

  $ build_pkg test
  foo:
  foo from test.files
  bar:
  bar from test.files
