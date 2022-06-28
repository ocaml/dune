  $ cat > dune-project << EOF
  > (lang dune 3.4)
  > EOF

It is an error to pass an empty list:

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (cat result)))
  > 
  > (rule
  >  (with-stdout-to result
  >   (cat)))
  > EOF

  $ dune runtest
  File "dune", line 8, characters 2-7:
  8 |   (cat)))
        ^^^^^
  Error: Not enough arguments for cat
  [1]

The cat action supports several files.

  $ echo "file a" > a
  $ echo "file b" > b
  $ echo "file c" > c

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (cat result)))
  > 
  > (rule
  >  (with-stdout-to result
  >   (cat a b c)))
  > EOF

  $ dune runtest
  file a
  file b
  file c

This requires 3.4.

  $ cat > dune-project << EOF
  > (lang dune 3.3)
  > EOF

  $ dune runtest
  File "dune", line 8, characters 2-13:
  8 |   (cat a b c)))
        ^^^^^^^^^^^
  Error: Passing several arguments to 'cat' is only available since version 3.4
  of the dune language. Please update your dune-project file to have (lang dune
  3.4).
  [1]
