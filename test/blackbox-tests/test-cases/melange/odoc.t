Test docs generation with Melange

  $ export DUNE_SANDBOX=none
  $ cat <<EOF > dune-project
  > (lang dune 3.8)
  > (using melange 0.1)
  > (package (name foo))
  > EOF

  $ cat <<EOF > dune
  > (library
  >  (name foo)
  >  (public_name foo)
  >  (modes :standard melange))
  > EOF
  > touch foo.ml bar.ml

Works for "universal" libraries

  $ dune build @doc

  $ cat _build/default/_doc/_mlds/foo/index.mld
  {0 foo index}
  {1 Library foo}
  The entry point of this library is the module:
  {!module-Foo}.

Works for Melange-only libraries

  $ cat <<EOF > dune
  > (library
  >  (name foo)
  >  (public_name foo)
  >  (modes melange))
  > EOF

  $ dune build @doc

  $ cat _build/default/_doc/_mlds/foo/index.mld
  {0 foo index}
  {1 Library foo}
  The entry point of this library is the module:
  {!module-Foo}.
