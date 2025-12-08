Test that autolocking correctly handles patches (reproduces issue #12851).

  $ . ./helpers.sh
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
  Error:
  open(_build/.sandbox/02c1ce8fe253c0ce60076b0735635d4e/_private/default/.pkg/foo.0.0.1-e9afc14c9b0a9025a9d7339fe72ba00d/source/fix.patch): No such file or directory
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-e9afc14c9b0a9025a9d7339fe72ba00d/target
  [1]
