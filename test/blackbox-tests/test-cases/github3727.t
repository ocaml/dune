This bug demonstrates a distinction between public & private names.

When -p was used, private names would disappear as they would be filtered by the
stanza filter. This behavior is incorrect and private names should remain
visible regardless if the stanzas were filtered.

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > (package (name foo))
  > (package (name bar))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name private_foo)
  >  (public_name foo.bar)
  >  (modules private_foo))
  > 
  > (executable
  >  (public_name bin)
  >  (libraries private_foo)
  >  (modules bin)
  >  (package bar))
  > EOF

  $ cat >bin.ml <<EOF
  > print_endline Private_foo.secret
  > EOF

  $ cat >private_foo.ml <<EOF
  > let secret = "private_foo"
  > EOF

  $ dune exec ./bin.exe
  private_foo

  $ rm -rf _build
  $ dune build -p foo
  $ dune install foo --prefix ./_install --display=short
  Installing _install/lib/foo/META
  Installing _install/lib/foo/bar/private_foo.a
  Installing _install/lib/foo/bar/private_foo.cma
  Installing _install/lib/foo/bar/private_foo.cmi
  Installing _install/lib/foo/bar/private_foo.cmt
  Installing _install/lib/foo/bar/private_foo.cmx
  Installing _install/lib/foo/bar/private_foo.cmxa
  Installing _install/lib/foo/bar/private_foo.ml
  Installing _install/lib/foo/dune-package
  Installing _install/lib/foo/bar/private_foo.cmxs

  $ export OCAMLPATH=$PWD/_install/lib
  $ dune build -p bar
  $ _build/install/default/bin/bin
  private_foo
