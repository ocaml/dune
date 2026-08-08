Marking modules as private

  $ make_dune_project 3.24

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foolib)
  >  (private_modules Baz.Foo))
  > EOF

  $ mkdir baz
  $ touch baz/foo.ml

Qualified references in module fields are version gated too:

  $ dune build
  File "dune", line 4, characters 18-25:
  4 |  (private_modules Baz.Foo))
                        ^^^^^^^
  Error: Using qualified module references is only available since version 3.25
  of the dune language. Please update your dune-project file to have (lang dune
  3.25).
  [1]

  $ make_dune_project 3.25
  $ dune build
