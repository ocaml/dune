Tests for promoting with non existent reference
-----------------------------------------------
# Diffing should do as if it is an empty file

  $ cat x-non-existent
  cat: x-non-existent: No such file or directory
  [1]

  $ dune build @blah-non-existent
  File "x-non-existent", line 1, characters 0-0:
  Error: Files _build/default/x-non-existent and _build/default/x.gen differ.
  [1]

  $ dune promote
  Promoting _build/default/x.gen to x-non-existent.

  $ cat x-non-existent
  toto
