Test that parse errors in the run error produce the expected error message.

  $ echo "(lang dune 3.11)" > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (target foo.txt)
  >  (action
  >   (run ())))
  > EOF

  $ dune build foo.txt 2>&1 | head -n 1
  Error: exception Stack overflow
