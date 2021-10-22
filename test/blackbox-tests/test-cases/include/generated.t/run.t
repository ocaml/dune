  $ echo "(lang dune 3.0)" > dune-project

  $ dune build @print
  File "sub/dune", line 1, characters 0-20:
  1 | (include ../sub.inc)
      ^^^^^^^^^^^^^^^^^^^^
  Error: File sub.inc doesn't exist in source tree.
  [1]

  $ echo "(using generated_include 0.1)" >> dune-project

  $ dune build @print
  done in sub 
  include in generated include in sub
  From executable
