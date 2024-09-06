This test won't work with MSVC as it's using GCC and clang CLI
parameters.

The flag should be present in the command, as we want it in the
:standard set.

We don't actually test that the compiler outputs colors, just that
Dune correctly passes the flag when required, and doesn't when it's
not.

test that we pass the flag
==========================

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ dune rules -m stub.o | grep -ce "-fdiagnostics-color=always"
  1

test color flag disabled
========================

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs
  >   (flags :standard \ -fdiagnostics-color=always)
  >   (language c) (names stub)))
  > EOF

  $ dune rules -m stub.o | grep -ce "-fdiagnostics-color=always"
  0
  [1]

test that we correctly filter out the color codes
=================================================

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs
  >   (language c) (names stub)))
  > EOF

We check there is no ESC [ sequence.

  $ dune build 2>&1 | grep `printf '\033\\['`
  [1]
