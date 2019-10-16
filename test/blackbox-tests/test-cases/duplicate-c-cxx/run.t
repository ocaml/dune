c_names and cxx_names with overlapping names in the same stanza
  $ dune build --root same-stanza @all
  Entering directory 'same-stanza'
  File "dune", line 4, characters 12-15:
  4 |  (cxx_names foo))
                  ^^^
  Error: Multiple sources map to the same object name "foo":
  - foo.c
  - foo.cpp
  This is not allowed; please rename them.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]

c_names with overlapping names in different stanzas
  $ dune build --root diff-stanza @all
  Entering directory 'diff-stanza'
  File "dune", line 4, characters 10-13:
  4 |  (c_names foo))
                ^^^
  Error: Multiple sources map to the same object name "foo":
  - foo.c
  - foo.cpp
  This is not allowed; please rename them.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]
