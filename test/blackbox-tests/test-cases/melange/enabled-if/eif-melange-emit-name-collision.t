Using same melange.emit target in two contexts

  $ mkdir -p a b
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (using melange 0.1)
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
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF
  $ cat > a/foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ cat > b/dune << EOF
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > b/foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune build
