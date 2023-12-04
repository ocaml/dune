# Test installation display output

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
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

dune install / uninstall should not output any messages by default:

  $ dune install --prefix prefix -p foo
  $ dune uninstall --prefix prefix -p foo

and for comparison here is the output with --display short:

  $ dune install --prefix another_prefix -p foo --display short
  Installing another_prefix/lib/foo/META
  Installing another_prefix/lib/foo/dune-package
  Installing another_prefix/lib/foo/foo.a
  Installing another_prefix/lib/foo/foo.cma
  Installing another_prefix/lib/foo/foo.cmi
  Installing another_prefix/lib/foo/foo.cmt
  Installing another_prefix/lib/foo/foo.cmx
  Installing another_prefix/lib/foo/foo.cmxa
  Installing another_prefix/lib/foo/foo.ml
  Installing another_prefix/lib/foo/foo.cmxs
  $ dune uninstall --prefix another_prefix -p foo --display short
  Deleting another_prefix/lib/foo/META
  Deleting another_prefix/lib/foo/dune-package
  Deleting another_prefix/lib/foo/foo.a
  Deleting another_prefix/lib/foo/foo.cma
  Deleting another_prefix/lib/foo/foo.cmi
  Deleting another_prefix/lib/foo/foo.cmt
  Deleting another_prefix/lib/foo/foo.cmx
  Deleting another_prefix/lib/foo/foo.cmxa
  Deleting another_prefix/lib/foo/foo.ml
  Deleting another_prefix/lib/foo/foo.cmxs
  Deleting empty directory another_prefix/lib/foo
