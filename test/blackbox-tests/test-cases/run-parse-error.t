Test that parse errors in the run error produce the expected error message.

See #9529.

  $ echo "(lang dune 3.11)" > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (target foo.txt)
  >  (action
  >   (run ())))
  > EOF

  $ dune build foo.txt
  File "dune", line 4, characters 7-9:
  4 |   (run ())))
             ^^
  Error: Unexpected list
  [1]
