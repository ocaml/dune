Using same melange.emit target in two contexts, where the stanzas are defined
in the same dune file

  $ make_melange_project 3.13 0.1

  $ make_two_context_workspace
  $ cat > dune << EOF
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "default"))
  >  (emit_stdlib false))
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "alt-context"))
  >  (emit_stdlib false))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune build
