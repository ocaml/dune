Test docs generation with Melange

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

Fails for Melange-only libraries

  $ cat <<EOF > dune
  > (library
  >  (name foo)
  >  (public_name foo)
  >  (modes melange))
  > EOF

  $ dune build @doc
  File ".foo.objs/byte/_unknown_", line 1, characters 0-0:
  Error: No rule found for .foo.objs/byte/foo.cmt
  File ".foo.objs/byte/_unknown_", line 1, characters 0-0:
  Error: No rule found for .foo.objs/byte/foo__.cmt
  File ".foo.objs/byte/_unknown_", line 1, characters 0-0:
  Error: No rule found for .foo.objs/byte/foo__Bar.cmt
  [1]
