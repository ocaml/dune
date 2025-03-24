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

Without any consumers of the libraries (both are built in separate folders)

  $ dune build a/foo.cma b/foo.cma

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
  File "a/dune", lines 1-2, characters 0-21:
  1 | (library
  2 |  (name foo))
  Error: Library with name "foo" is already defined in b/dune:1. Either change
  one of the names, or enable them conditionally using the 'enabled_if' field.
  [1]
