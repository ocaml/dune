Generate cstubs for a "vendored" library.

We have a dummy C library hosted entirely in the 'vendor' directory and use
the ctypes instrumentation and description language to generate bindings for
it.

This is the version that builds into a library and tests multiple function
description modules.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ TARGET=./stubgen/vendor
  $ mkdir -p $TARGET && install $LIBEX/*example* $TARGET
  $ dune exec ./example.exe
  6
