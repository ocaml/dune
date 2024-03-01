Using same library name in two contexts, where the libraries are defined
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
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --display=short
  File "dune", line 4, characters 0-69:
  4 | (library
  5 |  (name foo)
  6 |  (enabled_if (= %{context_name} "alt-context")))
  Error: Library "foo" appears for the second time in this directory
  [1]

For public libraries

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo)
  >  (enabled_if (= %{context_name} "default")))
  > (library
  >  (name foo)
  >  (public_name baz.foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF

  $ dune build
  File "dune", line 1, characters 0-0:
  Error: Module "Foo" is used in several stanzas:
  - dune:1
  - dune:5
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]
