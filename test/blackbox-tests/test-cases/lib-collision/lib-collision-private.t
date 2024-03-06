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
  Error: Library foo is defined twice:
  - a/dune:1
  - b/dune:1
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
  Error: Library foo is defined twice:
  - a/dune:1
  - b/dune:1
  [1]
