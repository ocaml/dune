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

  $ dune install --prefix prefix --display short
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

Test that we can cope with the situation where a destination is an empty
directory instead of a file.

  $ rm -rf prefix

  $ mkdir -p prefix/lib/foo/foo.a
  $ dune install --prefix prefix --display short
  Installing prefix/lib/foo/META
  Installing prefix/lib/foo/dune-package
  Deleting empty directory prefix/lib/foo/foo.a
  Installing prefix/lib/foo/foo.a
  Installing prefix/lib/foo/foo.cma
  Installing prefix/lib/foo/foo.cmi
  Installing prefix/lib/foo/foo.cmt
  Installing prefix/lib/foo/foo.cmx
  Installing prefix/lib/foo/foo.cmxa
  Installing prefix/lib/foo/foo.ml
  Installing prefix/lib/foo/foo.cmxs

Test the error message if a destination is a non-empty directory instead of a file.

  $ rm -rf prefix

  $ mkdir -p prefix/lib/foo/foo.a
  $ touch prefix/lib/foo/foo.a/file
  $ dune install --prefix prefix --display short
  Installing prefix/lib/foo/META
  Installing prefix/lib/foo/dune-package
  Error: Please delete non-empty directory prefix/lib/foo/foo.a manually.
  [1]

Test the error message if a destination is a file instead of a directory.

  $ rm -rf prefix
  $ mkdir -p prefix/lib; touch prefix/lib/foo
  $ dune install --prefix prefix --display short
  Installing prefix/lib/foo/META
  Error: Please delete file prefix/lib/foo manually.
  [1]
