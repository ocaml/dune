Showcase that using same library name in two workspaces is not possible at the moment

  $ mkdir -p a b
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > 
  > (context default)
  > 
  > (context
  >  (opam
  >   (switch $(opam switch show))
  >   (name alt-context)))
  > EOF
  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF
  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF

  $ dune build
  Error: Library foo is defined twice:
  - b/dune:1
  - a/dune:1
  [1]
