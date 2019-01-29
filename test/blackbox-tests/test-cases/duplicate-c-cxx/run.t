c_names and cxx_names with overlapping names in the same stanza
  $ dune build --root same-stanza @all
  Entering directory 'same-stanza'
  Multiple rules generated for _build/default/foo$ext_obj:
  - dune:3
  - dune:4
  [1]

c_names with overlapping names in different stanzas
  $ dune build --root diff-stanza @all
  Entering directory 'diff-stanza'
  Multiple rules generated for _build/default/foo$ext_obj:
  - dune:9
  - dune:4
  [1]
