Using same library name in two contexts

For private libraries

  $ mkdir -p a b
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package (name bar) (allow_empty))
  > (package (name baz) (allow_empty))
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > 
  > (context default)
  > 
  > (context
  >  (default
  >   (name alt-context)))
  > EOF
  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF
  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > b/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build

For public libraries

  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (public_name baz.foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF

  $ dune build

Mixing public and private libraries

  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (public_name baz.foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF

  $ dune build
