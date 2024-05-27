Public libraries using the same library name, in the same context, defined in
different folders.

  $ mkdir -p a b

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package (name bar) (allow_empty))
  > EOF

  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name bar)
  >  (public_name bar.foo))
  > EOF

Without any consumers of the libraries

  $ dune build
  File "b/dune", lines 1-3, characters 0-44:
  1 | (library
  2 |  (name bar)
  3 |  (public_name bar.foo))
  Error: Public library bar.foo is defined twice:
  - a/dune:1
  - b/dune:1
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
  File "b/dune", lines 1-3, characters 0-44:
  1 | (library
  2 |  (name bar)
  3 |  (public_name bar.foo))
  Error: Public library bar.foo is defined twice:
  - a/dune:1
  - b/dune:1
  [1]
