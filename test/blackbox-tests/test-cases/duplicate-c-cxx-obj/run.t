This test showcases that although libraries can technically have non overlapping
stubs names, things are still broken if their .o files overlap:

  $ dune build --root diff-stanza @all
  Entering directory 'diff-stanza'
  Error: Multiple rules generated for _build/default/foo$ext_obj:
  - dune:4
  - dune:9
  [1]

Another form of this bug is if the same source is present in different
directories. In this case, the rules are fine, but this is probably not what the
user intended.

  $ dune build --root same-stanza @all
  Entering directory 'same-stanza'
  File "dune", line 5, characters 14-21:
  5 |  (c_names foo sub/foo))
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
  Error: Multiple C sources for the same object "foo":
  - foo.c
  - sub/foo.c
  This is not allowed; please rename them.
  [1]
