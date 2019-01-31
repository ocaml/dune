  $ dune build @foo
  File "src/dune", line 4, characters 10-17:
  4 |  (c_names stubs/x))
                ^^^^^^^
  Warning: relative part of stub are no longer necessary and are ignored.
  File "src/dune", line 4, characters 10-17:
  4 |  (c_names stubs/x))
                ^^^^^^^
  Error: x does not exist as a C source. One of x.c must be present
  [1]
