Using same executable name in two contexts, where the executables are defined
in the same dune file

  $ make_dune_project 3.13

  $ make_two_context_workspace
  $ cat > dune << EOF
  > (executable
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > (executable
  >  (name foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune build
