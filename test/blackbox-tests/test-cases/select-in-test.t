We test the (select) field of the (libraries) field of the test stanza.

  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ cat > dune <<EOF
  > (test
  >  (name test)
  >  (modules test)
  >  (libraries
  >   (select test.ml from
  >    (unix -> test.unix.ml)
  >    (!unix -> test.nounix.ml))))
  > EOF

  $ cat > test.unix.ml <<EOF
  > let () = print_endline "Test: Unix was found!"
  > EOF
  $ cat > test.nounix.ml <<EOF
  > let () = print_endline "Test: Unix was not found!"
  > EOF

The select field does not pick up the module sources for the test stanza
correctly. This is a bug.

  $ dune runtest  
  File "dune", line 3, characters 10-14:
  3 |  (modules test)
                ^^^^
  Error: Module Test doesn't exist.
  [1]
