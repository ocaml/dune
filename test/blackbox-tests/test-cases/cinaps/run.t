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
  > "EOF"
  sh: line 4: warning: here-document at line 0 delimited by end-of-file (wanted `EOF')

  $ dune runtest --auto-promote
  File "test.ml", line 1, characters 0-0:
  Files _build/default/test.ml and _build/default/test.ml.cinaps-corrected differ.
  Promoting _build/default/test.ml.cinaps-corrected to test.ml.
  [1]
  $ cat test.ml
  (*$ print_endline "\nhello" *)
  hello
  (*$*)
  let x = 1
  "EOF"
