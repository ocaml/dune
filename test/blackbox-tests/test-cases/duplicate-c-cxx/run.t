c_names and cxx_names with overlapping names in the same stanza
  $ dune build --root same-stanza @all
  Entering directory 'same-stanza'
  File "dune", line 4, characters 12-15:
  4 |  (cxx_names foo))
                  ^^^
  Error: foo.cpp source file is invalid because foo.c exists
  [1]

c_names with overlapping names in different stanzas
  $ dune build --root diff-stanza @all
  Entering directory 'diff-stanza'
  File "dune", line 9, characters 10-13:
  9 |  (c_names foo))
                ^^^
  Error: This c stub is already used in another stanza:
  - dune:4
  
  [1]
