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
  Error: Library foo is defined twice:
  - dune:3
  - dune:1
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
  Error: Library foo is defined twice:
  - dune:3
  - dune:1
  [1]
