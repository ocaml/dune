Generate cstubs for a "vendored" library.

We have a dummy C library hosted entirely in the 'vendor' directory and use
the ctypes instrumentation and description language to generate bindings for
it.

This is the version that builds into an executable.

This test is identical to exe-vendored.t except it uses preamble instead of
include to require the example.h header.

  $ dune exec ./example.exe
  4
