Public libraries using the same library name, in the same context, defined in
the same folder.

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package (name bar) (allow_empty))
  > (package (name baz) (allow_empty))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo))
  > (library
  >  (name foo)
  >  (public_name baz.foo))
  > EOF

Without any consumers of the libraries

  $ dune build
  Error:
  File "dune", line 1, characters 0-44:
  Error: A library with name "foo" is defined in two folders: _build/default
  and _build/default. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  
  [1]

With some consumer

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo))
  > (library
  >  (name foo)
  >  (public_name baz.foo))
  > (executable
  >  (name main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  Error:
  File "dune", line 1, characters 0-44:
  Error: A library with name "foo" is defined in two folders: _build/default
  and _build/default. Either change one of the names, or enable them
  conditionally using the 'enabled_if' field.
  
  File "dune", line 1, characters 0-0:
  Error: Module "Main" is used in several stanzas:
  - dune:1
  - dune:4
  - dune:7
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]
