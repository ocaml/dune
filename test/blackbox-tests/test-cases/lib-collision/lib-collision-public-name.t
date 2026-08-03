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

  $ rm dune
  $ mkdir baz bar
  $ cat > bar/dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo))
  > EOF
  $ cat > baz/dune << EOF
  > (library
  >  (name foo)
  >  (public_name baz.foo))
  > EOF

  $ dune build

