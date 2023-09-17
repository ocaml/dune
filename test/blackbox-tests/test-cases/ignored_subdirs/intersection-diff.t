Removing (and) should evaluate to the empty list

We need the extra (or ()) to avoid some simplfication steps.

  $ cat >dune <<EOF
  > (dirs * \ (and))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > EOF

  $ mkdir sub
  $ cat >sub/dune <<EOF
  > ignore me
  > EOF

  $ dune build
