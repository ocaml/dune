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
  Multiple rules generated for _build/default/foo$ext_obj:
  - dune:9
  - dune:4
  [1]
