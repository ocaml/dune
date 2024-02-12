Showcase that using same library name in two workspaces is possible for private libraries

  $ mkdir -p a b
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
  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF
  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > b/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build

But not for public libraries

  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (public_name baz.foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF

  $ dune build
  Error: Library foo is defined twice:
  - b/dune:3
  - a/dune:3
  [1]


In the same context

  $ cat > a/dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo))
  > EOF

If no public lib is available, the build finishes fine as there are no consumers of the libraries

  $ dune build

Let's add an exe to consume the library to trigger the error

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  File "a/dune", line 1, characters 0-21:
  1 | (library
  2 |  (name foo))
  Error: A library with name "foo" is defined in two folders:
  _build/alt-context/b and _build/alt-context/a. Either change one of the
  names, or enable them conditionally using the 'enabled_if' field.
  File "a/dune", line 1, characters 0-21:
  1 | (library
  2 |  (name foo))
  Error: A library with name "foo" is defined in two folders: _build/default/b
  and _build/default/a. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  [1]
