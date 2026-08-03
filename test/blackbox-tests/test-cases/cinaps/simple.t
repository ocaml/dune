Test of cinaps integration

  $ make_cinaps_project 1.11 1.0

  $ cat > dune <<EOF
  > (cinaps (files *.ml))
  > EOF

  $ cat > test.ml <<"EOF"
  > (*$ print_endline "\nhello" *)
  > (*$*)
  > let x = 1
  > EOF

The cinaps actions should be attached to the runtest alias:

  $ dune runtest --diff-command diff 2>&1 | sed -E 's/[^ ]+sh/\$sh/'
  File "test.ml", line 1, characters 0-0:
  1a2
  > hello
  [1]

but also to the cinaps alias:

  $ dune build @cinaps --diff-command diff 2>&1 | sed -E 's/[^ ]+sh/\$sh/'
  File "test.ml", line 1, characters 0-0:
  1a2
  > hello
  [1]

The cinaps stanza offers a promotion workflow:

  $ dune runtest --auto-promote
  File "test.ml", line 1, characters 0-0:
  --- test.ml
  +++ test.ml.cinaps-corrected
  @@ -1,3 +1,4 @@
   (*$ print_endline "\nhello" *)
  +hello
   (*$*)
   let x = 1
  Promoting _build/default/test.ml.cinaps-corrected to test.ml.

  $ cat test.ml
  (*$ print_endline "\nhello" *)
  hello
  (*$*)
  let x = 1
