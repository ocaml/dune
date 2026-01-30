  $ cat >dune <<EOF
  > (library
  >  (modes byte)
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > type a = { a : int }
  > and b = { a : int ; b : bool }
  > EOF

  $ make_dune_project 3.12
  $ dune build foo.cma
  File "foo.ml", line 2, characters 10-19:
  2 | and b = { a : int ; b : bool }
                ^^^^^^^^^
  Error (warning 30 [duplicate-definitions]): the label a is defined in both types a and b.
  [1]

  $ make_dune_project 3.13
  $ dune build foo.cma
