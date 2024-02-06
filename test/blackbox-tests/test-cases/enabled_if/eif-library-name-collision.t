Showcase that using same library name in two workspaces is possible for private libraries

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

But not for public libraries

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
  Error: Library foo is defined twice:
  - b/dune:3
  - a/dune:3
  [1]


In the same context

  $ cat > a/dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ dune build
