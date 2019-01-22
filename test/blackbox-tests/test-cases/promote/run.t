General tests
--------------------------

  $ printf titi > x

  $ dune build --display short @blah
  File "x", line 1, characters 0-0:
  Files _build/default/x and _build/default/x.gen differ.
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
  Files _build/default/x and _build/default/x.gen differ.
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
  Files _build/default/x and _build/default/x.gen differ.
  File "y", line 1, characters 0-0:
  Files _build/default/y and _build/default/y.gen differ.
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
  Files _build/default/x and _build/default/x.gen differ.
  File "y", line 1, characters 0-0:
  Files _build/default/y and _build/default/y.gen differ.
  [1]
  $ rm -f _build/default/x.gen
  $ dune promote
  Promoting _build/default/x.gen to x.
  Error: exception Sys_error("_build/default/x.gen: No such file or directory")
  Backtrace:
  Raised by primitive operation at file "stdlib.ml", line 390, characters 28-54
  Called from file "src/stdune/io.ml", line 74, characters 17-37
  Called from file "src/promotion.ml", line 86, characters 6-31
  Called from file "map.ml", line 295, characters 20-25
  Called from file "src/promotion.ml", line 93, characters 4-44
  Called from file "src/promotion.ml", line 124, characters 11-41
  Called from file "vendor/cmdliner/src/cmdliner_term.ml", line 25, characters 19-24
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 116, characters 32-39
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 146, characters 18-36
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 261, characters 22-48
  Called from file "bin/main.ml", line 177, characters 10-51
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
