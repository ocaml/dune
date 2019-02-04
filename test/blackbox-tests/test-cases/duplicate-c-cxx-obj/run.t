This test showcases that although libraries can technically have non overlapping
stubs names, things are still broken if their .o files overlap:
  $ dune build --root diff-stanza @all
  Entering directory 'diff-stanza'
  Multiple rules generated for _build/default/foo$ext_obj:
  - dune:9
  - dune:4
  [1]

Another form of this bug is if the same source is present in different
directories. In this case, the rules are fine, but this is probably not what the
user intended.
  $ dune build --root same-stanza @all
  Entering directory 'same-stanza'
  File "dune", line 1, characters 0-0:
  Error: c file foo appears in several directories:
  - .
  - sub
  [1]
