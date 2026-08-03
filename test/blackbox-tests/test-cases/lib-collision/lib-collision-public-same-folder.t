Public libraries using the same library name, in the same context, defined in
the same folder.

  $ make_bar_baz_packages_project

  $ write_duplicate_foo_public_libraries

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
