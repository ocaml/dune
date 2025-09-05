Test installation for melange libraries in multiple contexts.
The expectation is that libraries that don't collide (e.g. a melange and a
native library) can be installed in the same context.

Create a package named foo

  $ mkdir -p a b
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > (package (name foo))
  > EOF

Define libraries in multiple contexts

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > (context default)
  > (context
  >  (default
  >   (name melange)))
  > EOF
  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (public_name foo.native)
  >  (enabled_if (= %{context_name} "default")))
  > (library
  >  (name foo)
  >  (public_name foo.melange)
  >  (modes melange)
  >  (enabled_if (= %{context_name} "melange")))
  > EOF
  $ cat > a/foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune build

  $ mkdir -p out/man


Try installing both

  $ dune install foo --prefix out
  Error: Cannot specify '--prefix' or '--libdir' when installing into multiple
  contexts!
  [1]
