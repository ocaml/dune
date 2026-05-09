Reproduction case for #1342. Check that when the user edits files in
_build, things are rebuild as expected.

Note that this is no longer supported. Users are not expected to edit things in
the _build directory.

This case now specifically checks that a source-backed build copy is repaired
when the corresponding build artifact is edited.

  $ echo 42 > x
  $ dune build x
  $ cat _build/default/x
  42
  $ chmod u+w _build/default/x
  $ echo 0 > _build/default/x
  $ dune build x
  $ cat _build/default/x
  42
