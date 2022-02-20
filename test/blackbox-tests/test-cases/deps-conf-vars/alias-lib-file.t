  $ echo >dune <<EOF
  > (alias
  >  (name default)
  >  (deps %{lib:foo:theories/a})
  >  (action (echo "deps: %{deps}\n")))
  > EOF
  $ echo "(lang dune 1.6)" > dune-project
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
