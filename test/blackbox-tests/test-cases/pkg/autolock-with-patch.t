Test that autolocking correctly handles patches (reproduces issue #12851).

  $ mkrepo

  $ mkdir foo
  $ cat > foo/foo.ml <<EOF
  > This is wrong
  > EOF
  $ tar cf foo.tar foo
  $ rm -rf foo

  $ mkpkg foo <<EOF
  > build: ["cat" "foo.ml"]
  > patches: ["fix.patch"]
  > url { src: "$PWD/foo.tar" }
  > EOF

  $ mkdir -p $mock_packages/foo/foo.0.0.1/files
  $ cat >$mock_packages/foo/foo.0.0.1/files/fix.patch <<EOF
  > diff --git a/foo.ml b/foo.ml
  > --- a/foo.ml
  > +++ b/foo.ml
  > @@ -1,1 +1,1 @@
  > -This is wrong
  > +This is right
  > EOF

  $ solve foo
  Solution for dune.lock:
  - foo.0.0.1

  $ build_pkg foo
  This is right

  $ rm -rf dune.lock
  $ enable_pkg
  $ build_pkg foo
