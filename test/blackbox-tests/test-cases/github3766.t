  $ cat >dune <<EOF
  > (library
  >  (name nomodules))
  > EOF
  $ echo "(lang dune 2.8)" > dune-project
  $ dune build @all
  File "dune", line 1, characters 0-27:
  1 | (library
  2 |  (name nomodules))
  Warning: This library does not contain any modules. This should be specified
  explicitly by setting an empty field: (modules).
