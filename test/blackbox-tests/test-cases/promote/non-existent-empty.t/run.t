Tests for promoting with non existent reference
-----------------------------------------------
# Diffing should do as if it is an empty file

  $ cat x-non-existent
  cat: x-non-existent: No such file or directory
  [1]

  $ dune build @blah-non-existent

  $ dune promote

  $ cat x-non-existent
  cat: x-non-existent: No such file or directory
  [1]
