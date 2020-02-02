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
  Error: exception Sys_error("does-not-exist/x: No such file or directory")
  Backtrace:
  Raised by primitive operation at file "stdlib.ml", line 324, characters 29-55
  Called from file "src/stdune/io.ml", line 184, characters 8-126
  Re-raised at file "src/stdune/io.ml", line 189, characters 8-23
  Called from file "src/dune/artifact_substitution.ml", line 383, characters 15-48
  Called from file "src/fiber/fiber.ml", line 229, characters 16-19
  Called from file "src/dune/build_system.ml", line 1648, characters 8-1023
  Called from file "src/fiber/fiber.ml", line 114, characters 10-15
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
