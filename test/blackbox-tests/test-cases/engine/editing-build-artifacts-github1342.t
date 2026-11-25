Reproduction case for #1342. Edits to files in _build are unsupported: Dune
owns these files and trusts its recorded metadata.

A real source change must still replace the build copy, even if it was edited.

  $ echo 42 > x
  $ dune build x
  $ cat _build/default/x
  42
  $ chmod u+w _build/default/x
  $ echo 0 > _build/default/x
  $ echo 43 > x
  $ dune build x
  $ cat _build/default/x
  43
