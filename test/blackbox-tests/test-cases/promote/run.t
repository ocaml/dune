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

The correction file stays on the filesystem even after promotion and
can be depended on by other actions.

  $ cat _build/default/x.gen
  toto

  $ printf titi > x
  $ dune build --display short @blah x.gen.copy
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/x.gen differ.
  [1]
  $ cat _build/default/x.gen.copy
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

Test for (promote (into ...)) + (enabled_if %{ignoring_promoted_rules}
----------------------------------------------------------------------

  $ dune build into+ignoring
  $ dune clean
  $ dune build into+ignoring --ignore-promoted-rules

Reproduction case for #3069
---------------------------

  $ mkdir 3069 && cd 3069
  $ echo "(lang dune 2.0)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (action (with-stdout-to x (echo bar)))
  >  (mode (promote (into does-not-exist))))
  > EOF
  $ dune build ./x
  File "dune", line 3, characters 22-36:
  3 |  (mode (promote (into does-not-exist))))
                            ^^^^^^^^^^^^^^
  Error: directory "does-not-exist" does not exist
  [1]
