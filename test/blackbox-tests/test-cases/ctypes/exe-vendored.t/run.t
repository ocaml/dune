Generate cstubs for a "vendored" library.

We have a dummy C library hosted entirely in the 'vendor' directory and use
the ctypes instrumentation and description language to generate bindings for
it.

This is the version that builds into an executable.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ TARGET=./vendor
  $ mkdir -p $TARGET && install $LIBEX/*example* $TARGET
Ctypes 0.3 should honor dependencies when sandboxing is requested externally.

  $ DUNE_SANDBOX=symlink dune exec ./example.exe
  4
  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select(.args.process_args | any(contains("c_cout_generated_functions")))
  >   | (.args.dir | contains(".sandbox"))
  >   ][0]'
  false
