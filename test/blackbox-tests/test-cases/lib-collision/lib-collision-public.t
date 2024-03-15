Public libraries using the same library name, in the same context, defined in
different folders.

  $ mkdir -p a b

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package (name bar) (allow_empty))
  > (package (name baz) (allow_empty))
  > EOF

  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (public_name baz.foo))
  > EOF

Without any consumers of the libraries

  $ dune build
  Error:
  File "b/dune", line 1, characters 0-44:
  Error: A library with name "foo" is defined in two folders: _build/default/a
  and _build/default/b. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  
  -> required by _build/install/default/lib/bar/foo/foo.a
  -> required by _build/default/bar.install
  -> required by alias all
  -> required by alias default
  [1]

With some consumer

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  Error:
  File "b/dune", line 1, characters 0-44:
  Error: A library with name "foo" is defined in two folders: _build/default/a
  and _build/default/b. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  
  -> required by _build/install/default/lib/bar/foo/foo.a
  -> required by _build/default/bar.install
  -> required by alias all
  -> required by alias default
  File "b/dune", line 1, characters 0-44:
  1 | (library
  2 |  (name foo)
  3 |  (public_name baz.foo))
  Error: A library with name "foo" is defined in two folders: _build/default/a
  and _build/default/b. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  [1]
