This test won't work with MSVC as it's using GCC and clang CLI
parameters.

We have to force Dune outputting colors, for that we use
`CLICOLOR_FORCE=1`.

The flag should be present in the command, as we want it in the
:standard set.

We don't actually test that the compiler outputs colors, just that
Dune correctly passes the flag when required, and doesn't when it's
not.

test, color enabled, color flag default (enabled)
=================================================

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ CLICOLOR_FORCE=1 dune rules -m stub.o | grep -ce "-fdiagnostics-color=always"
  1

test, color enabled, color flag disabled
========================================

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs
  >   (flags :standard \ -fdiagnostics-color=always)
  >   (language c) (names stub)))
  > EOF

  $ CLICOLOR_FORCE=1 dune rules -m stub.o | grep -ce "-fdiagnostics-color=always"
  0
  [1]

test, color disabled, color flag default (enabled)
==================================================

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs
  >   (language c) (names stub)))
  > EOF

  $ CLICOLOR=0 dune rules -m stub.o | grep -ce "-fdiagnostics-color=always"
  0
  [1]
