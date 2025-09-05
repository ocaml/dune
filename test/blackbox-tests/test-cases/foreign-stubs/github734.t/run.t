  $ dune build @foo
  File "src/dune", line 4, characters 10-17:
  4 |  (c_names stubs/x))
                ^^^^^^^
  Error: Relative part of stub is not necessary and should be removed. To
  include sources in subdirectories, use the (include_subdirs ...) stanza.
  [1]
