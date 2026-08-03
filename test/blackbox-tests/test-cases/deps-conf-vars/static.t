Expands static library artifact variables in dependencies.

  $ cat >dune <<EOF
  > (alias
  >  (name default)
  >  (deps %{lib:foo:foo.cma})
  >  (action (echo "deps: %{deps}\n")))
  > EOF
  $ make_dune_project 1.6
  $ touch foo.opam
  $ mkdir foo && touch foo/foo.ml
  $ cat >foo/dune << EOF
  > (library (public_name foo))
  > EOF

  $ dune build
  deps: ../install/default/lib/foo/foo.cma
