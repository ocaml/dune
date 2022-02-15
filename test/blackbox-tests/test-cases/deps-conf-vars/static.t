  $ cat >dune <<EOF
  > (alias
  >  (name default)
  >  (deps %{lib:foo:foo.cma})
  >  (action (echo "deps: %{deps}\n")))
  > EOF
  $ echo "(lang dune 1.6)" > dune-project
  $ touch foo.opam
  $ mkdir foo && touch foo/foo.ml
  $ cat >foo/dune << EOF
  > (library (public_name foo))
  > EOF

  $ dune build
  deps: ../install/default/lib/foo/foo.cma
