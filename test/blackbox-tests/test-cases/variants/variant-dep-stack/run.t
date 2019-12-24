Example of a dependency stack where a forbidden library is pulled via a variant.
Dune display the variant that pulled in the implementation that requires the
forbidden library.

  $ dune build
  File "dune", line 16, characters 22-31:
  16 |  (forbidden_libraries forbidden)
                             ^^^^^^^^^
  Error: Library "forbidden" was pulled in.
  -> required by library "impl" in _build/default via variant "foo"
  [1]
