  $ cat >dune <<EOF
  > (library
  >  (modes byte)
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > type a = { a : int }
  > and b = { a : int ; b : bool }
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.12)
  > EOF
  $ dune build foo.cma
  File "foo.ml", line 2, characters 10-19:
  2 | and b = { a : int ; b : bool }
                ^^^^^^^^^
  Error (warning 30 [duplicate-definitions]): the label a is defined in both types a and b.
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ dune build foo.cma
