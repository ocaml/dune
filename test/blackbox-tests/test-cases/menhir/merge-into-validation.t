The merge_into field must name a file in the stanza directory.

  $ make_menhir_project 3.21 3.0

  $ cat >dune <<EOF
  > (menhir
  >  (merge_into ../parser)
  >  (modules parser))
  > EOF

  $ dune build
  File "dune", line 2, characters 13-22:
  2 |  (merge_into ../parser)
                   ^^^^^^^^^
  Error: The merge_into field must be a filename without directory components
  [1]

  $ cat >dune <<EOF
  > (menhir
  >  (merge_into "")
  >  (modules parser))
  > EOF

  $ dune build
  File "dune", line 2, characters 13-15:
  2 |  (merge_into "")
                   ^^
  Error: The merge_into field must be a filename without directory components
  [1]
