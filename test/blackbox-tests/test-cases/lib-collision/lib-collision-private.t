Private libraries using the same library name, in the same context, defined in
different folders.

  $ mkdir -p a b

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > a/dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo))
  > EOF

Without any consumers of the libraries

  $ dune build
  Error:
  File "b/dune", line 1, characters 0-21:
  Error: A library with name "foo" is defined in two folders: _build/default/a
  and _build/default/b. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  
  -> required by alias default
  [1]

With some consumer of the library

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
  File "b/dune", line 1, characters 0-21:
  Error: A library with name "foo" is defined in two folders: _build/default/a
  and _build/default/b. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  
  -> required by alias default
  File "b/dune", line 1, characters 0-21:
  1 | (library
  2 |  (name foo))
  Error: A library with name "foo" is defined in two folders: _build/default/a
  and _build/default/b. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  [1]
