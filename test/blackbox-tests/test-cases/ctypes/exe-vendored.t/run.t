Generate cstubs for a "vendored" library.

We have a dummy C library hosted entirely in the 'vendor' directory and use
the ctypes instrumentation and description language to generate bindings for
it.

This is the version that builds into an executable.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ TARGET=./vendor
  $ mkdir -p $TARGET && install $LIBEX/*example* $TARGET
Ctypes stub generation is sandboxed and honors its declared dependencies.

  $ dune exec ./example.exe
  4
  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select((.args.target_files // [])
  >            | any(startswith("_build/default/examplelib__")))
  >   | (.args.dir | contains(".sandbox"))
  >   ] | unique'
  [true]
