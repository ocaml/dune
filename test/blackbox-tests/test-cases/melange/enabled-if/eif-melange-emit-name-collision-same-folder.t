Using same melange.emit target in two contexts, where the stanzas are defined
in the same dune file

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
