# Test that .cmxs are installed with the executable bit set

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (package (name foo))
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (public_name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build @install

  $ test -x _build/install/default/lib/foo/foo.cmxs

  $ dune install --prefix prefix
  Installing prefix/lib/foo/META
  Installing prefix/lib/foo/dune-package
  Installing prefix/lib/foo/foo.a
  Installing prefix/lib/foo/foo.cma
  Installing prefix/lib/foo/foo.cmi
  Installing prefix/lib/foo/foo.cmt
  Installing prefix/lib/foo/foo.cmx
  Installing prefix/lib/foo/foo.cmxa
  Installing prefix/lib/foo/foo.ml
  Installing prefix/lib/foo/foo.cmxs

  $ test -x prefix/lib/foo/foo.cmxs

Test the error message if a destination is a directory instead of a file.

  $ rm -rf prefix

  $ mkdir -p prefix/lib/foo/foo.a
  $ dune install --prefix prefix
  Installing prefix/lib/foo/META
  Installing prefix/lib/foo/dune-package
  Deleting prefix/lib/foo/foo.a
  Error: Is a directory
  Installing prefix/lib/foo/foo.a
  Error: prefix/lib/foo/foo.a: Is a directory
  [1]

Test the error message if a destination is a file instead of a directory.

  $ rm -rf prefix
  $ mkdir -p prefix/lib; touch prefix/lib/foo
  $ dune install --prefix prefix
  Installing prefix/lib/foo/META
  Error: prefix/lib/foo/META: Not a directory
  [1]
