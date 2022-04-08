Test of cinaps integration

  $ cat > dune-project <<EOF
  > (lang dune 1.11)
  > (using cinaps 1.0)
  > EOF

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

but also to the cinaps alias:

  $ dune build @cinaps --diff-command diff 2>&1 | sed -E 's/[^ ]+sh/\$sh/'
  File "test.ml", line 1, characters 0-0:
  1a2
  > hello

The cinaps stanza offers a promotion workflow:

  $ dune runtest --auto-promote
  File "test.ml", line 1, characters 0-0:
  Error: Files _build/default/test.ml and
  _build/default/test.ml.cinaps-corrected differ.
  Promoting _build/default/test.ml.cinaps-corrected to test.ml.
  [1]

  $ cat test.ml
  (*$ print_endline "\nhello" *)
  hello
  (*$*)
  let x = 1
