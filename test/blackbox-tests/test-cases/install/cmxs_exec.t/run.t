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
