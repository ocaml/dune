We can nested modules virtual
  $ dune build @all
  File "impl/dune", line 1, characters 17-26:
  1 | (include_subdirs qualified)
                       ^^^^^^^^^
  Error: Unknown value qualified
  Hint: did you mean unqualified?
  File "vlib/dune", line 1, characters 17-26:
  1 | (include_subdirs qualified)
                       ^^^^^^^^^
  Error: Unknown value qualified
  Hint: did you mean unqualified?
  [1]
