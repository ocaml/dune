Private libraries using the same library name, in the same context, defined in
the same folder.

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name foo))
  > (library
  >  (name foo))
  > EOF

Without any consumers of the libraries

  $ dune build
  File "dune", line 3, characters 0-21:
  3 | (library
  4 |  (name foo))
  Error: Library "foo" has the same private name as another library in this
  directory
  [1]

With some consumer of the library

  $ cat > dune << EOF
  > (library
  >  (name foo))
  > (library
  >  (name foo))
  > (executable
  >  (name main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  File "dune", line 3, characters 0-21:
  3 | (library
  4 |  (name foo))
  Error: Library "foo" has the same private name as another library in this
  directory
  [1]
