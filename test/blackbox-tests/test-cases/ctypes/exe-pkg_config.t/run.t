Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

Then generate cstubs for it, build an executable that uses those cstubs, and
run the executable that tests the library through the cstubs.

  $ LIBEX=$(realpath "$PWD/../libexample")

Pkg-config runs in a sandbox starting with Dune language 3.25.

  $ PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix" \
  > dune build ./example.exe
  $ dune trace cat | jq_dune -sc '
  >   [ .[]
  >   | processes
  >   | select(.args.prog | basename == "pkg-config")
  >   | select(.args.process_args | index("--cflags"))
  >   | (.args.dir | contains(".sandbox"))
  >   ][0]'
  true

  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix" dune exec ./example.exe
  4
