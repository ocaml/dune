Additional files overlaid on top of the source can be found in the
%pkg.files/ directory:

  $ mkdir test-source
  $ touch test-source/foo

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<EOF
  > (source
  >  (copy $PWD/test-source))
  > (build
  >  (system "echo foo:; cat foo; echo bar:; cat bar"))
  > EOF

  $ mkdir dune.lock/test.files
  $ cat >dune.lock/test.files/foo <<EOF
  > foo from test.files
  > EOF
  $ cat >dune.lock/test.files/bar <<EOF
  > bar from test.files
  > EOF

  $ dune build .pkg/test/target/
  foo:
  foo from test.files
  bar:
  bar from test.files
