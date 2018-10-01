Reproduction case for #1342. Check that when the user edits files in
_build, things are rebuild as expected.

  $ echo 42 > x
  $ dune build x
  $ cat _build/default/x
  42
  $ echo 0 > _build/default/x
  $ dune build x
  $ cat _build/default/x
  42
