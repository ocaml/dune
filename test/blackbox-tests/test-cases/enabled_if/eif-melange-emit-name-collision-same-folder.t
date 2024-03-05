Using same melange.emit target in two contexts, where the stanzas are defined
in the same dune file

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (using melange 0.1)
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
  $ cat > dune << EOF
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "default")))
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune build
  File "dune", line 4, characters 0-76:
  4 | (melange.emit
  5 |  (target foo)
  6 |  (enabled_if (= %{context_name} "alt-context")))
  Error: Target "foo" appears for the second time in this directory
  [1]
