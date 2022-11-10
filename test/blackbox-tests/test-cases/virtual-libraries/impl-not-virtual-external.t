Test that trying to implement external libraries that aren't virtual results in
an appropriate error message.

  $ mkdir external
  $ echo "(lang dune 2.5)" > external/dune-project
  $ touch external/foodummy.opam
  $ cat >external/dune <<EOF
  > (library (public_name foodummy))
  > EOF
  $ dune build --root external @install
  Entering directory 'external'
  Leaving directory 'external'
  $ mkdir test
  $ echo "(lang dune 2.5)" > test/dune-project
  $ cat >test/dune <<EOF
  > (library (implements foodummy) (name bar))
  > EOF
  $ OCAMLPATH=$PWD/external/_build/install/default/lib dune build --root test @all
  Entering directory 'test'
  File "dune", line 1, characters 21-29:
  1 | (library (implements foodummy) (name bar))
                           ^^^^^^^^
  Error: Library "foodummy" is not virtual. It cannot be implemented by "bar".
  Leaving directory 'test'
  [1]
