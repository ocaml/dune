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
  File "dune", line 4, characters 0-44:
  4 | (library
  5 |  (name foo)
  6 |  (public_name baz.foo))
  Error: Library "foo" appears for the second time in this directory
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
  File "dune", line 4, characters 0-44:
  4 | (library
  5 |  (name foo)
  6 |  (public_name baz.foo))
  Error: Library "foo" appears for the second time in this directory
  [1]
