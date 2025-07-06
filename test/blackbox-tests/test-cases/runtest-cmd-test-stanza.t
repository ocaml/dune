Testing the interaction of dune runtest and (tests).

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (test
  >  (name test))
  > EOF

  $ cat > test.ml <<EOF
  > let () = assert false
  > EOF

Dune runtest is able to run a test from a (tests) stanza.

  $ dune runtest test
  File "dune", line 2, characters 7-11:
  2 |  (name test))
             ^^^^
  Fatal error: exception Assert_failure("test.ml", 1, 9)
  [1]

Mistakes are caught and hints are suggested:

  $ dune runtest tst
  Error: "tst" does not match any known test.
  Hint: did you mean test?
  [1]

