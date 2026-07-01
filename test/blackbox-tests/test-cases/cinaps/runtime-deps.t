Runtime dependencies for running cinaps

  $ make_cinaps_project 3.5 1.1

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
  --- test.ml
  +++ test.ml.cinaps-corrected
  @@ -1,2 +1 @@
  -(*$ let f = open_in "foo" in print_endline (input_line f); close_in f *)
  -(*)
  +(*$ let f = open_in "foo" in print_endline (input_line f); close_in f *)hello world
  Promoting _build/default/test.ml.cinaps-corrected to test.ml.
  [1]
  $ cat test.ml
  (*$ let f = open_in "foo" in print_endline (input_line f); close_in f *)hello world
