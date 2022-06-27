The cat action supports several files.

  $ cat > dune-project << EOF
  > (lang dune 3.4)
  > EOF

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
  File "dune", line 8, characters 9-10:
  8 |   (cat a b c)))
               ^
  Error: Too many argument for cat
  [1]

This requires 3.4.

  $ cat > dune-project << EOF
  > (lang dune 3.3)
  > EOF

  $ dune runtest
  File "dune", line 8, characters 9-10:
  8 |   (cat a b c)))
               ^
  Error: Too many argument for cat
  [1]
