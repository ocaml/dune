General tests
--------------------------

  $ printf titi > x

  $ dune build --display short @blah
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/x.gen differ.
  [1]
  $ cat x
  titi

  $ dune promote --display short
  Promoting _build/default/x.gen to x.
  $ cat x
  toto

  $ dune build --display short @blah
  $ cat x
  toto

Otherwise this test fails on OSX
  $ dune clean --display short

  $ printf titi > x
  $ dune build --display short @blah --auto-promote
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/x.gen differ.
  Promoting _build/default/x.gen to x.
  [1]
  $ cat x
  toto
  $ dune build --display short @blah
  $ cat x
  toto

Test single file promotion
--------------------------

  $ printf a > x
  $ printf a > y
  $ dune build --display short @blah @blah2
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/x.gen differ.
  File "y", line 1, characters 0-0:
  Error: Files _build/default/y and _build/default/y.gen differ.
  [1]
  $ dune promote x
  Promoting _build/default/x.gen to x.
  $ cat x
  toto
  $ cat y
  a
  $ dune promote y
  Promoting _build/default/y.gen to y.
  $ cat x
  toto
  $ cat y
  titi
  $ dune promote x y
  Warning: Nothing to promote for x.
  Warning: Nothing to promote for y.

Reproduction case for #1772
---------------------------

  $ printf a > x
  $ printf a > y
  $ dune build --display short @blah @blah2
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/x.gen differ.
  File "y", line 1, characters 0-0:
  Error: Files _build/default/y and _build/default/y.gen differ.
  [1]
  $ rm -f _build/default/x.gen
  $ dune promote
  Skipping promotion of _build/default/x.gen to x as the file is missing.
  Promoting _build/default/y.gen to y.

Tests for promote-into
----------------------

  $ mkdir -p subdir
  $ dune build promoted
  $ cat subdir/promoted
  Hello, world!

Test for (promote (only ...))
-----------------------------

Only "only1" should be promoted in the source tree:

  $ dune build only2
  $ ls -1 only*
  only1
