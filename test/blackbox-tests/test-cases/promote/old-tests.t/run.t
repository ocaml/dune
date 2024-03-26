General tests
--------------------------

  $ printf titi > x

  $ dune build @blah
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/x.gen differ.
  [1]
  $ cat x
  titi

  $ dune promote
  Promoting _build/default/x.gen to x.
  $ cat x
  toto

  $ dune build @blah
  $ cat x
  toto

The correction file stays on the filesystem even after promotion and
can be depended on by other actions.

We disable this test for OSX because it's flaky
  $ if [ "$(uname)" = "Darwin" ]
  > then
  >   echo toto
  > else
  >   cat _build/default/x.gen
  > fi
  toto

  $ printf titi > x
  $ dune build @blah x.gen.copy
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/x.gen differ.
  [1]
  $ cat _build/default/x.gen.copy
  toto

Otherwise this test fails on OSX
  $ dune clean

  $ printf titi > x
  $ dune build @blah --auto-promote
  File "x", line 1, characters 0-0:
  Error: Files _build/default/x and _build/default/x.gen differ.
  Promoting _build/default/x.gen to x.
  [1]
  $ cat x
  toto
  $ dune build @blah --wait-for-filesystem-clock
  $ cat x
  toto

Test single file promotion
--------------------------

  $ printf a > x
  $ printf a > y
  $ dune build @blah @blah2
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
  $ dune build @blah @blah2
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

Dune restores only1 if it's deleted from the source tree

  $ rm only1
  $ dune build only2
  $ ls -1 only*
  only1

Dune restores only1 if it's modified in the source tree

  $ cat only1
  0
  $ echo 1 > only1
  $ dune build only2
  $ cat only1
  0

Test for (promote (into ...)) + (enabled_if %{ignoring_promoted_rules}
----------------------------------------------------------------------

  $ dune build into+ignoring
  $ ls -1 subdir/into*
  subdir/into+ignoring

  $ dune clean
  $ dune build into+ignoring --ignore-promoted-rules
  $ ls -1 _build/default/into*
  _build/default/into+ignoring

Reproduction case for #3069
---------------------------

  $ mkdir 3069 && cd 3069
  $ echo "(lang dune 2.0)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (action (with-stdout-to x (echo bar)))
  >  (mode (promote (into dir))))
  > EOF
  $ dune build ./x
  File "dune", line 3, characters 22-25:
  3 |  (mode (promote (into dir))))
                            ^^^
  Error: Directory "dir" does not exist. Please create it manually.
  -> required by _build/default/x
  [1]

Now test the case where dir exists but is a file

  $ touch dir

  $ dune build ./x
  File "dune", line 3, characters 22-25:
  3 |  (mode (promote (into dir))))
                            ^^^
  Error: "dir" is not a directory.
  -> required by _build/default/x
  [1]
