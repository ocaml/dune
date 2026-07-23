Using same executable name in two contexts

  $ mkdir -p a b
  $ make_dune_project 3.13

  $ make_two_context_workspace
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
