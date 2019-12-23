Implementation of library from another project is not allowed when tagged with
variant.

  $ dune build
  File "prj2/dune", line 4, characters 13-20:
  4 |  (implements vlibfoo)
                   ^^^^^^^
  Error: Implementation "impl" cannot have variant "somevariant" for virtual
  library "vlibfoo" as it is already defined for implementation "real-impl".
  [1]
