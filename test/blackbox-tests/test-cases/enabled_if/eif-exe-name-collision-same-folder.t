Using same executable name in two contexts, where the executables are defined
in the same dune file

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

  $ dune build --display=short
  File "dune", line 4, characters 0-72:
  4 | (executable
  5 |  (name foo)
  6 |  (enabled_if (= %{context_name} "alt-context")))
  Error: Executable "foo" appears for the second time in this directory
  [1]

For public exes

  $ cat > dune << EOF
  > (executable
  >  (public_name foo)
  >  (package bar)
  >  (enabled_if (= %{context_name} "default")))
  > (executable
  >  (public_name foo)
  >  (package baz)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF

  $ dune build
  File "dune", line 5, characters 0-94:
  5 | (executable
  6 |  (public_name foo)
  7 |  (package baz)
  8 |  (enabled_if (= %{context_name} "alt-context")))
  Error: Executable "foo" appears for the second time in this directory
  [1]
