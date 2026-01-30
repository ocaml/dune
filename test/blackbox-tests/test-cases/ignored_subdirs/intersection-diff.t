Removing (and) should evaluate to the empty list

We need the extra (or ()) to avoid some simplfication steps.

  $ cat >dune <<EOF
  > (dirs * \ (and))
  > EOF

  $ make_dune_project 3.8

  $ mkdir sub
  $ cat >sub/dune <<EOF
  > ignore me
  > EOF

  $ dune build
