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
  >  (enabled_if (= %{context_name} "default")))
  > (melange.emit
  >  (target foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune build
  File "dune", line 1, characters 0-0:
  Error: Module "Foo" is used in several stanzas:
  - dune:1
  - dune:4
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]
