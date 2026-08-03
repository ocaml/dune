----------------------------------------------------------------------------------
Test that duplicate foreign objects results in an error

  $ make_dune_project 3.5

  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (extra_objects foo foo))
  > EOF

  $ dune build
  File "dune", line 3, characters 16-19:
  3 |  (extra_objects foo foo))
                      ^^^
  Error: Duplicate object name: foo. Already appears at:
  - dune:3
  [1]
