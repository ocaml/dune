Marking modules as private

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foolib)
  >  (private_modules baz/foo))
  > EOF

  $ mkdir baz
  $ touch baz/foo.ml

  $ dune build
  File "dune", line 4, characters 18-25:
  4 |  (private_modules baz/foo))
                        ^^^^^^^
  Error: Module Baz/foo doesn't exist.
  [1]
