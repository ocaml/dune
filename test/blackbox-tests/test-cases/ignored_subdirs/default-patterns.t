Test our default ignore pattern for directories:

  $ make_dune_project 3.8

  $ mkdir .ignoreme _ignoreme p
  $ touch .ignoreme/a _ignoreme/a p/a

  $ dune build .ignoreme/a
  Error: Don't know how to build .ignoreme/a
  [1]
  $ dune build _ignoreme/a
  Error: Don't know how to build _ignoreme/a
  [1]
  $ dune build p/a
