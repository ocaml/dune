Expands library artifact variables in alias dependencies.

  $ echo >dune <<EOF
  > (alias
  >  (name default)
  >  (deps %{lib:foo:theories/a})
  >  (action (echo "deps: %{deps}\n")))
  > EOF
  $ make_dune_project 1.6
  $ touch foo.opam
  $ mkdir foo && touch foo/a
  $ cat >foo/dune <<EOF
  > (library
  >  (public_name foo))
  > 
  > (install
  >  (section lib)
  >  (package foo)
  >  (files (a as theories/a)))
  > EOF
  $ dune build
