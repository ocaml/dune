Generate cstubs for an example executable exercising the `return_errno` and the
default `ignore_errno` errno policies, build an executable, and run it.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX"  PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix"  dune exec ./example.exe
  6
