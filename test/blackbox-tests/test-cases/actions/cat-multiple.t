  $ make_dune_project 3.4

  $ write_cat_result_rule() {
  > local args="$1"
  > cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (cat result)))
  > 
  > (rule
  >  (with-stdout-to result
  >   (cat${args})))
  > EOF
  > }

It is an error to pass an empty list:

  $ write_cat_result_rule ""

  $ dune runtest
  File "dune", line 8, characters 2-7:
  8 |   (cat)))
        ^^^^^
  Error: Not enough arguments for "cat"
  [1]

The cat action supports several files.

  $ echo "file a" > a
  $ echo "file b" > b
  $ echo "file c" > c

  $ write_cat_result_rule " a b c"

  $ dune runtest
  file a
  file b
  file c

This requires 3.4.

  $ make_dune_project 3.3

  $ dune runtest
  File "dune", line 8, characters 2-13:
  8 |   (cat a b c)))
        ^^^^^^^^^^^
  Error: Passing several arguments to 'cat' is only available since version 3.4
  of the dune language. Please update your dune-project file to have (lang dune
  3.4).
  [1]
