Using same melange.emit target in two contexts

  $ mkdir -p a b
  $ make_melange_project 3.13 0.1

  $ make_two_context_workspace
  $ cat > a/dune << EOF
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "default"))
  >  (emit_stdlib false))
  > EOF
  $ cat > a/foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ cat > b/dune << EOF
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "alt-context"))
  >  (emit_stdlib false))
  > EOF
  $ cat > b/foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune build
