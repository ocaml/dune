This test showcases that although libraries can technically have non overlapping
stubs names, things are still broken if their .o files overlap:

  $ cat >same-stanza/dune <<EOF
  > (library
  >  (name foo)
  >  (c_names foo sub/foo))
  > EOF

  $ dune build --root diff-stanza @all
  Entering directory 'diff-stanza'
  File "dune", line 4, characters 10-13:
  4 |  (c_names foo))
                ^^^
  Error: Multiple definitions for the same object file "foo$ext_obj". See another
  definition at dune:9.
  Hint: You can avoid the name clash by renaming one of the objects, or by
  placing it into a different directory.
  [1]

Another form of this bug is if the same source is present in different
directories. In this case, the rules are fine, but this is probably not what the
user intended.

  $ dune build --root same-stanza @all
  Entering directory 'same-stanza'
  File "dune", line 3, characters 14-21:
  3 |  (c_names foo sub/foo))
                    ^^^^^^^
  Error: Relative part of stub is not necessary and should be removed. To
  include sources in subdirectories, use the (include_subdirs ...) stanza.
  [1]

  $ cat >same-stanza/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name foo)
  >  (c_names foo))
  > EOF

  $ dune build --root same-stanza @all
  Entering directory 'same-stanza'
  File "dune", line 4, characters 10-13:
  4 |  (c_names foo))
                ^^^
  Error: Multiple sources map to the same object name "foo":
  - foo.c
  - sub/foo.c
  This is not allowed; please rename them or remove "foo" from object names.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]
