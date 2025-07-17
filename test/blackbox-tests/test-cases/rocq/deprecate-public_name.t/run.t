public_name field is deprecated
  $ cat > dune << EOF
  > (rocq.theory
  >  (name foo)
  >  (public_name Foo))
  > EOF

  $ dune build
  File "dune", line 3, characters 2-13:
  3 |  (public_name Foo))
        ^^^^^^^^^^^
  Error: Unknown field "public_name"
  [1]

both package and public_name field is an error
  $ cat > dune << EOF
  > (rocq.theory
  >  (name foo)
  >  (public_name Foo)
  >  (package Foo))
  > EOF

  $ dune build
  File "dune", line 3, characters 2-13:
  3 |  (public_name Foo)
        ^^^^^^^^^^^
  Error: Unknown field "public_name"
  [1]
