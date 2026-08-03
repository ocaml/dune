Unless they introduce private runtime dependencies:
  $ dune build
  File "dune", line 16, characters 7-18:
  16 |   (pps private_ppx))
              ^^^^^^^^^^^
  Error: Library "private_runtime_dep" is private, it cannot be a dependency of
  a public library. You need to give "private_runtime_dep" a public name or add
  a (package ...) field to its stanza.
  [1]
