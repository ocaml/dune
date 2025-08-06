Additional files overlaid on top of the source can be found in the
%pkg.files/ directory:

  $ . ./helpers.sh

  $ mkdir test-source
  $ touch test-source/foo

  $ make_lockdir
  $ cat > ${default_lock_dir}/test.pkg <<EOF
  > (version 0.0.1)
  > (source
  >  (copy $PWD/test-source))
  > (build
  >  (system "echo foo:; cat foo; echo bar:; cat bar"))
  > EOF

  $ mkdir ${default_lock_dir}/test.files
  $ cat > ${default_lock_dir}/test.files/foo <<EOF
  > foo from test.files
  > EOF
  $ cat > ${default_lock_dir}/test.files/bar <<EOF
  > bar from test.files
  > EOF

  $ build_pkg test
  foo:
  foo from test.files
  bar:
  bar from test.files
