Runtime dependencies for running cinaps

  $ cat > dune-project <<EOF
  > (lang dune 3.5)
  > (using cinaps 1.1)
  > EOF

  $ cat > foo <<EOF
  > hello world
  > EOF

  $ cat > dune <<EOF
  > (cinaps
  >  (files *.ml)
  >  (runtime_deps foo))
  > EOF

  $ cat > test.ml <<EOF
  > (*$ let f = open_in "foo" in print_endline (input_line f); close_in f *)
  > (*$*)
  > EOF

  $ dune build @cinaps --auto-promote
  File "test.ml", line 1, characters 0-0:
  Error: Files _build/default/test.ml and
  _build/default/test.ml.cinaps-corrected differ.
  Promoting _build/default/test.ml.cinaps-corrected to test.ml.
  [1]
  $ cat test.ml
  (*$ let f = open_in "foo" in print_endline (input_line f); close_in f *)hello world
