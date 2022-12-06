When the rule action targets cannot be inferred by dune, we should make it explicit
in the error message:

  $ dune build
  File "dune", line 1, characters 0-36:
  1 | (rule (action (system "something")))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule has no targets specified
  [1]
