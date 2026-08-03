----------------------------------------------------------------------------------
* (foreign_library ...) is unavailable before Dune 2.0.

  $ make_dune_project 1.0
  $ mkdir -p lib

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (language c)
  >  (names add))
  > EOF

  $ dune build
  File "lib/dune", lines 1-3, characters 0-44:
  1 | (foreign_library
  2 |  (language c)
  3 |  (names add))
  Error: 'foreign_library' is only available since version 2.0 of the dune
  language. Please update your dune-project file to have (lang dune 2.0).
  [1]

----------------------------------------------------------------------------------
* (foreign_library ...) is available in Dune 2.0.
* "archive_name" is a required field.

  $ make_dune_project 2.0

  $ dune build
  File "lib/dune", lines 1-3, characters 0-44:
  1 | (foreign_library
  2 |  (language c)
  3 |  (names add))
  Error: Field "archive_name" is missing
  [1]
