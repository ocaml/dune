Using same executable name in two contexts

For private exes

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
  > (executable
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF
  $ cat > a/foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ cat > b/dune << EOF
  > (executable
  >  (name foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > b/foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune build

For public exes

  $ cat > a/dune << EOF
  > (executable
  >  (public_name foo)
  >  (package bar)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ cat > b/dune << EOF
  > (executable
  >  (public_name foo)
  >  (package baz)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF

  $ dune build
