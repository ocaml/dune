----------------------------------------------------------------------------------
Test that duplicate foreign objects results in an error

  $ echo "(lang dune 3.5)" > dune-project

  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (foreign_objects foo foo))
  > EOF

  $ dune build
  File "dune", line 3, characters 18-21:
  3 |  (foreign_objects foo foo))
                        ^^^
  Error: Duplicate object name: foo. Already appears at:
  - dune:3
  [1]
