Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

Then generate cstubs for it, build an executable that uses those cstubs, and
run the executable that tests the library through the cstubs.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix" dune exec ./example.exe
  4
