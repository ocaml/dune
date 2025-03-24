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
  File "dune", line 5, characters 7-10:
  5 |  (name foo)
             ^^^
  Error: Library "foo" appears for the second time in this directory
  [1]

With some consumer

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (modules)
  >  (public_name bar.foo))
  > (library
  >  (name foo)
  >  (modules)
  >  (public_name baz.foo))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  File "dune", line 6, characters 7-10:
  6 |  (name foo)
             ^^^
  Error: Library "foo" appears for the second time in this directory
  [1]
